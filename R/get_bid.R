bid_country_name <- function(code) {
  country_map <- c(
    AR = "Argentina", BS = "Bahamas", BB = "Barbados", BZ = "Belice",
    BO = "Bolivia", BR = "Brasil", CL = "Chile", CO = "Colombia",
    CR = "Costa Rica", DO = "Republica Dominicana", EC = "Ecuador",
    SV = "El Salvador", GT = "Guatemala", GY = "Guyana", HT = "Haiti",
    HN = "Honduras", JM = "Jamaica", MX = "Mexico", NI = "Nicaragua",
    PA = "Panama", PY = "Paraguay", PE = "Peru", SR = "Surinam",
    TT = "Trinidad y Tobago", UY = "Uruguay", VE = "Venezuela",
    RG = "Regional"
  )

  code <- toupper(as.character(code %||% NA_character_))
  dplyr::coalesce(unname(country_map[code]), code)
}

bid_pick_translation <- function(translations) {
  if (is.null(translations) || !length(translations)) {
    return(list())
  }

  languages <- purrr::map_chr(
    translations,
    ~ as.character(.x[["languages_code"]] %||% "")
  )

  preferred <- match(TRUE, languages %in% c("es", "es-ES", "es-419"))
  if (is.na(preferred)) {
    preferred <- match(TRUE, languages %in% c("en", "en-US"))
  }
  if (is.na(preferred)) {
    preferred <- 1L
  }

  translations[[preferred]]
}

bid_value <- function(x, name, default = NA_character_) {
  value <- x[[name]]
  if (is.null(value) || length(value) == 0) {
    return(default)
  }
  if (length(value) > 1) {
    value <- value[[1]]
  }
  if (is.null(value) || identical(value, "")) {
    return(default)
  }
  value
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) {
    y
  } else {
    x
  }
}

get_bid <- function(limit = as.integer(Sys.getenv("BID_ROWS", unset = "300"))) {
  base_url <- "https://bidfa-admin.connectamericas.com/items/tenders"

  response <- httr2::request(base_url) |>
    httr2::req_user_agent("Mozilla/5.0 licitaciones-dashboard/1.0") |>
    httr2::req_url_query(
      limit = limit,
      sort = "-date_created",
      `filter[status][_neq]` = "expired",
      fields = paste(
        "id,status,date_created,date_updated,date_close,amount",
        "country.*,project_number,project_url,tender_url",
        "contact_email,contact_webpage,city,translations.*",
        sep = ","
      )
    ) |>
    httr2::req_timeout(30) |>
    httr2::req_perform()

  payload <- httr2::resp_body_json(response, simplifyVector = FALSE)
  items <- payload[["data"]]

  if (is.null(items) || !length(items)) {
    return(empty_licitaciones())
  }

  rows <- purrr::map_dfr(items, function(item) {
    translation <- bid_pick_translation(item[["translations"]])

    titulo <- bid_value(translation, "name",
      bid_value(item, "project_number", paste("Oportunidad BID", bid_value(item, "id")))
    )
    descripcion <- bid_value(translation, "contract_object", titulo)
    organismo <- bid_value(translation, "executing_unit", "Banco Interamericano de Desarrollo")

    country_code <- bid_value(item[["country"]] %||% list(), "code")
    enlace <- bid_value(item, "tender_url",
      bid_value(item, "project_url", "https://bidfortheamericas.connectamericas.com/")
    )

    tibble::tibble(
      id = paste0("bid-", bid_value(item, "id")),
      fuente = "BID",
      titulo = normalize_text(titulo),
      organismo = normalize_text(organismo),
      pais = bid_country_name(country_code),
      region = NA_character_,
      ciudad = normalize_text(bid_value(item, "city")),
      fecha_publicacion = parse_date_any(bid_value(item, "date_created")),
      fecha_cierre = parse_date_any(bid_value(item, "date_close")),
      dias_restantes = NA_integer_,
      monto = as.numeric(clean_amount(bid_value(item, "amount", NA_real_)))[1],
      moneda = NA_character_,
      tipo_postulante = "no especificado",
      categoria = normalize_text(bid_value(item, "project_number", "licitacion")),
      tematica = "consultoria",
      descripcion = normalize_text(descripcion),
      enlace = enlace,
      estado = dplyr::case_when(
        bid_value(item, "status") == "approved" ~ "activa",
        bid_value(item, "status") == "expired" ~ "cerrada",
        TRUE ~ as.character(bid_value(item, "status", "activa"))
      ),
      idioma = as.character(bid_value(translation, "languages_code", NA_character_)),
      fecha_extraccion = Sys.time()
    )
  })

  standardize_licitaciones(rows)
}
