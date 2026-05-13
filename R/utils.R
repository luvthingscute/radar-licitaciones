STANDARD_COLUMNS <- c(
  "id", "fuente", "titulo", "organismo", "pais", "region", "ciudad",
  "fecha_publicacion", "fecha_cierre", "dias_restantes", "monto", "moneda",
  "tipo_postulante", "categoria", "tematica", "descripcion", "enlace",
  "estado", "idioma", "fecha_extraccion"
)

SOURCE_REGISTRY <- tibble::tibble(
  fuente = c(
    "Mercado Publico",
    "PNUD / UNDP",
    "UNGM",
    "World Bank Procurement",
    "BID",
    "CEPAL"
  ),
  portal = c(
    "https://www.mercadopublico.cl/Home/BusquedaLicitacion",
    "https://procurement-notices.undp.org/search.cfm",
    "https://www.ungm.org/Public/Notice",
    "https://www.worldbank.org/en/projects-operations/procurement",
    "https://bidfortheamericas.connectamericas.com/",
    "https://www.cepal.org/es/acerca/adquisiciones"
  ),
  modo = c(
    "API",
    "Scraping HTML",
    "Endpoint AJAX publico",
    "API",
    "API interna publica",
    "Scraping HTML"
  ),
  nota = c(
    "API oficial ChileCompra con ticket publico o MERCADO_PUBLICO_TICKET.",
    "Portal publico de Procurement Notices.",
    "Busqueda publica de Procurement Opportunities.",
    "API publica de Procurement Notices.",
    "Extrae oportunidades desde el endpoint publico usado por BID for the Americas.",
    "Extrae documentos EOI/RFI publicados por CEPAL."
  )
)

ALL_SOURCES <- SOURCE_REGISTRY$fuente

empty_licitaciones <- function() {
  tibble::tibble(
    id = character(),
    fuente = character(),
    titulo = character(),
    organismo = character(),
    pais = character(),
    region = character(),
    ciudad = character(),
    fecha_publicacion = as.Date(character()),
    fecha_cierre = as.Date(character()),
    dias_restantes = integer(),
    monto = numeric(),
    moneda = character(),
    tipo_postulante = character(),
    categoria = character(),
    tematica = character(),
    descripcion = character(),
    enlace = character(),
    estado = character(),
    idioma = character(),
    fecha_extraccion = as.POSIXct(character())
  )
}

normalize_text <- function(x) {
  x |>
    dplyr::coalesce("") |>
    stringr::str_squish()
}

encode_query <- function(...) {
  utils::URLencode(stringr::str_squish(paste(..., collapse = " ")), reserved = TRUE)
}

source_fallback_url <- function(fuente, titulo = "", organismo = "") {
  purrr::pmap_chr(
    list(fuente = fuente, titulo = titulo, organismo = organismo),
    function(fuente, titulo, organismo) {
      query <- dplyr::case_when(
        stringr::str_detect(fuente, "Mercado Publico") ~ paste("site:mercadopublico.cl", titulo, organismo),
        stringr::str_detect(fuente, "PNUD|UNDP") ~ paste("site:procurement-notices.undp.org", titulo, organismo),
    stringr::str_detect(fuente, "UNGM") ~ paste("site:ungm.org/Public/Notice", titulo, organismo),
    stringr::str_detect(fuente, "World Bank") ~ paste("site:worldbank.org procurement notices", titulo, organismo),
    stringr::str_detect(fuente, "BID|IDB") ~ paste("site:iadb.org procurement notices", titulo, organismo),
    stringr::str_detect(fuente, "CEPAL") ~ paste("site:cepal.org adquisiciones expresiones interes", titulo, organismo),
    TRUE ~ paste(titulo, organismo, "procurement notice tender")
  )

      paste0("https://www.google.com/search?q=", encode_query(query))
    }
  )
}

source_portal_url <- function(fuente) {
  dplyr::case_when(
    stringr::str_detect(fuente, "Mercado Publico") ~ "https://www.mercadopublico.cl/Home/BusquedaLicitacion",
    stringr::str_detect(fuente, "PNUD|UNDP") ~ "https://procurement-notices.undp.org/search.cfm",
    stringr::str_detect(fuente, "UNGM") ~ "https://www.ungm.org/Public/Notice",
    stringr::str_detect(fuente, "World Bank") ~ "https://www.worldbank.org/en/projects-operations/procurement",
    stringr::str_detect(fuente, "BID|IDB") ~ "https://bidfortheamericas.connectamericas.com/",
    stringr::str_detect(fuente, "CEPAL") ~ "https://www.cepal.org/es/acerca/adquisiciones",
    TRUE ~ "https://www.google.com/search?q=procurement%20notice%20tender"
  )
}

parse_date_any <- function(x) {
  x <- as.character(x)
  x <- stringr::str_replace(x, "(\\d{1,2}[-/][A-Za-z]{3}[-/]\\d{2})(\\d{1,2}:)", "\\1 \\2")
  x <- stringr::str_extract(x, "\\d{1,2}[-/][A-Za-z]{3}[-/]\\d{4}|\\d{1,2}[-/][A-Za-z]{3}[-/]\\d{2}|\\d{1,2}[-/]\\d{1,2}[-/]\\d{2,4}|\\d{4}-\\d{1,2}-\\d{1,2}")
  parsed <- suppressWarnings(lubridate::parse_date_time(
    x,
    orders = c("ymd HMS", "ymd HM", "ymd", "dmy HMS", "dmy HM", "dmy", "mdy", "d-b-y", "d-b-Y"),
    tz = "America/Santiago"
  ))
  as.Date(parsed)
}

clean_amount <- function(x) {
  if (is.numeric(x)) {
    return(as.numeric(x))
  }

  parse_one <- function(value) {
    value <- as.character(value %||% NA_character_)
    if (is.na(value) || !nzchar(value)) {
      return(NA_real_)
    }

    value <- stringr::str_replace_all(value, "\\s+", "")
    value <- stringr::str_replace_all(value, "[^0-9,.-]", "")
    value <- stringr::str_replace(value, "^-+", "-")
    value <- stringr::str_replace_all(value, "(?<!^)-", "")

    if (!nzchar(value) || !stringr::str_detect(value, "\\d")) {
      return(NA_real_)
    }

    has_comma <- stringr::str_detect(value, ",")
    has_dot <- stringr::str_detect(value, "\\.")
    comma_count <- stringr::str_count(value, ",")
    dot_count <- stringr::str_count(value, "\\.")

    if (has_comma && has_dot) {
      last_comma <- max(gregexpr(",", value, fixed = TRUE)[[1]])
      last_dot <- max(gregexpr(".", value, fixed = TRUE)[[1]])
      if (last_comma > last_dot) {
        value <- stringr::str_replace_all(value, "\\.", "")
        value <- stringr::str_replace(value, ",", ".")
      } else {
        value <- stringr::str_replace_all(value, ",", "")
      }
    } else if (has_comma) {
      decimal_tail <- stringr::str_match(value, ",(\\d+)$")[, 2]
      if (comma_count == 1 && !is.na(decimal_tail) && stringr::str_detect(decimal_tail, "^0{3,}$")) {
        value <- stringr::str_replace(value, ",", ".")
      } else if (stringr::str_detect(value, ",\\d{1,2}$")) {
        value <- stringr::str_replace(value, ",", ".")
      } else {
        value <- stringr::str_replace_all(value, ",", "")
      }
    } else if (has_dot) {
      decimal_tail <- stringr::str_match(value, "\\.(\\d+)$")[, 2]
      if (dot_count == 1 && !is.na(decimal_tail) && stringr::str_detect(decimal_tail, "^0{3,}$")) {
        value <- value
      } else if (!stringr::str_detect(value, "\\.\\d{1,2}$")) {
        value <- stringr::str_replace_all(value, "\\.", "")
      }
    }

    suppressWarnings(as.numeric(value))
  }

  purrr::map_dbl(x, parse_one)
}

detect_currency <- function(text, fuente = NA_character_) {
  text <- normalize_text(text)
  detected <- dplyr::case_when(
    stringr::str_detect(text, stringr::regex("US\\$|U\\$S|\\bUSD\\b|dolares|dollars", ignore_case = TRUE)) ~ "USD",
    stringr::str_detect(text, stringr::regex("\\bEUR\\b|€", ignore_case = TRUE)) ~ "EUR",
    stringr::str_detect(text, stringr::regex("\\bCLP\\b|pesos chilenos", ignore_case = TRUE)) ~ "CLP",
    stringr::str_detect(text, stringr::regex("\\bUF\\b", ignore_case = TRUE)) ~ "UF",
    stringr::str_detect(text, stringr::regex("\\bGBP\\b|£", ignore_case = TRUE)) ~ "GBP",
    stringr::str_detect(text, stringr::regex("\\bCOP\\b", ignore_case = TRUE)) ~ "COP",
    stringr::str_detect(text, stringr::regex("\\bPEN\\b", ignore_case = TRUE)) ~ "PEN",
    stringr::str_detect(text, stringr::regex("\\bBRL\\b", ignore_case = TRUE)) ~ "BRL",
    stringr::str_detect(text, stringr::regex("\\bMXN\\b", ignore_case = TRUE)) ~ "MXN",
    stringr::str_detect(fuente %||% "", "Mercado Publico") ~ "CLP",
    TRUE ~ NA_character_
  )
  detected
}

extract_amount_from_text <- function(text) {
  text <- normalize_text(text)
  pattern <- stringr::regex(
    paste(
      "(US\\$|U\\$S|USD|EUR|€|CLP|UF|GBP|COP|PEN|BRL|MXN)\\s*\\$?\\s*\\d[\\d.,]*(\\s*(millones|million|millions))?",
      "\\d[\\d.,]*\\s*(US\\$|U\\$S|USD|EUR|€|CLP|UF|GBP|COP|PEN|BRL|MXN)(\\s*(millones|million|millions))?",
      sep = "|"
    ),
    ignore_case = TRUE
  )

  match <- stringr::str_extract(text, pattern)
  amount <- clean_amount(match)
  multiplier <- dplyr::if_else(
    stringr::str_detect(match %||% "", stringr::regex("millones|million|millions", ignore_case = TRUE)),
    1000000,
    1
  )
  amount <- amount * multiplier
  dplyr::if_else(!is.na(amount) & amount >= 1000, amount, NA_real_)
}

improve_amounts <- function(df) {
  if (is.null(df) || !nrow(df)) {
    return(df)
  }

  text <- paste(df$titulo, df$descripcion, df$categoria)
  extracted_amount <- extract_amount_from_text(text)
  detected_currency <- detect_currency(text, df$fuente)

  df |>
    dplyr::mutate(
      monto = dplyr::coalesce(as.numeric(monto), extracted_amount),
      monto = dplyr::if_else(
        fuente == "Mercado Publico" & !is.na(monto) & monto < 1000,
        NA_real_,
        monto
      ),
      moneda = dplyr::coalesce(dplyr::na_if(normalize_text(moneda), ""), detected_currency),
      moneda = dplyr::if_else(is.na(monto), NA_character_, moneda)
    )
}

col_or <- function(df, name, default = NA_character_) {
  if (name %in% names(df)) {
    df[[name]]
  } else if (length(default) == nrow(df)) {
    default
  } else {
    rep(default, nrow(df))
  }
}

calculate_days_remaining <- function(fecha_cierre) {
  as.integer(as.Date(fecha_cierre) - Sys.Date())
}

standardize_licitaciones <- function(df) {
  if (is.null(df) || !nrow(df)) {
    return(empty_licitaciones())
  }

  missing_cols <- setdiff(STANDARD_COLUMNS, names(df))
  for (col in missing_cols) {
    df[[col]] <- NA
  }

  df |>
    dplyr::mutate(
      dplyr::across(c(id, fuente, titulo, organismo, pais, region, ciudad, moneda,
                      tipo_postulante, categoria, tematica, descripcion, enlace,
                      estado, idioma), as.character),
      fecha_publicacion = as.Date(fecha_publicacion),
      fecha_cierre = as.Date(fecha_cierre),
      monto = as.numeric(monto),
      dias_restantes = dplyr::if_else(
        is.na(dias_restantes),
        calculate_days_remaining(fecha_cierre),
        as.integer(dias_restantes)
      ),
      fecha_extraccion = as.POSIXct(fecha_extraccion, tz = "America/Santiago")
    ) |>
    dplyr::select(dplyr::all_of(STANDARD_COLUMNS))
}

safe_source <- function(expr, source_name) {
  tryCatch(
    expr,
    error = function(e) {
      warning(sprintf("Fuente '%s' no disponible: %s", source_name, e$message), call. = FALSE)
      empty_licitaciones()
    }
  )
}

demo_licitaciones <- function(fuente) {
  today <- Sys.Date()
  titulos <- c(
    "Consultoria para estrategia de cambio climatico y datos GIS",
    "Servicio de infraestructura urbana y eficiencia energetica",
    "Asistencia tecnica para programa de salud y gobernanza"
  )

  tibble::tibble(
    id = paste0(tolower(gsub("[^A-Za-z0-9]", "_", fuente)), "-", seq_len(3)),
    fuente = fuente,
    titulo = titulos,
    organismo = c("Unidad de adquisiciones", "Programa internacional", "Banco multilateral"),
    pais = c("Chile", "Peru", "Colombia"),
    region = c("Metropolitana", NA, NA),
    ciudad = c("Santiago", "Lima", "Bogota"),
    fecha_publicacion = today - c(2, 5, 9),
    fecha_cierre = today + c(5, 14, 30),
    dias_restantes = c(5L, 14L, 30L),
    monto = c(NA_real_, 125000, 78000),
    moneda = c(NA_character_, "USD", "USD"),
    tipo_postulante = "no especificado",
    categoria = c("consultoria", "servicios", "asistencia tecnica"),
    tematica = "consultoria",
    descripcion = c(
      "Se requiere consultor individual o firma para analisis climatico y geoespacial.",
      "Contratacion de empresa para obras, energia e infraestructura urbana.",
      "Servicios de apoyo institucional, salud publica y fortalecimiento de capacidades."
    ),
    enlace = source_fallback_url(fuente, titulos, organismo),
    estado = "activa",
    idioma = c("es", "es", "es"),
    fecha_extraccion = Sys.time()
  ) |>
    standardize_licitaciones()
}

ensure_links <- function(df) {
  df |>
    dplyr::mutate(
      enlace = dplyr::if_else(
        is.na(enlace) | enlace == "",
        source_fallback_url(fuente, titulo, organismo),
        enlace
      )
    )
}
