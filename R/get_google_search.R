get_google_search <- function(
    api_key = Sys.getenv("GOOGLE_SEARCH_API_KEY", unset = ""),
    cx = Sys.getenv("GOOGLE_SEARCH_CX", unset = ""),
    query = Sys.getenv(
      "GOOGLE_SEARCH_QUERY",
      unset = paste(
        "licitacion OR tender OR procurement OR consultancy",
        "site:ungm.org OR site:procurement-notices.undp.org OR site:worldbank.org OR site:mercadopublico.cl"
      )
    )) {
  # Requiere Google Programmable Search Engine:
  # GOOGLE_SEARCH_API_KEY = API key
  # GOOGLE_SEARCH_CX = identificador del buscador
  # GOOGLE_SEARCH_QUERY = consulta opcional
  if (!nzchar(api_key) || !nzchar(cx)) {
    message("Google Custom Search no configurado. Usando datos demo enlazados.")
    return(demo_licitaciones("Google Search"))
  }

  resp <- tryCatch(
    httr2::request("https://www.googleapis.com/customsearch/v1") |>
      httr2::req_url_query(key = api_key, cx = cx, q = query, num = 10) |>
      httr2::req_timeout(30) |>
      httr2::req_perform(),
    error = function(e) {
      message("Google Custom Search no disponible: ", e$message)
      return(NULL)
    }
  )

  if (is.null(resp)) {
    return(demo_licitaciones("Google Search"))
  }

  raw <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  items <- tibble::as_tibble(raw$items %||% list())

  if (!nrow(items)) {
    return(empty_licitaciones())
  }

  tibble::tibble(
    id = digest::digest(items$link, algo = "xxhash32"),
    fuente = "Google Search",
    titulo = normalize_text(items$title),
    organismo = dplyr::case_when(
      stringr::str_detect(items$link, "mercadopublico") ~ "Mercado Publico",
      stringr::str_detect(items$link, "undp|procurement-notices") ~ "UNDP",
      stringr::str_detect(items$link, "ungm") ~ "UNGM",
      stringr::str_detect(items$link, "worldbank") ~ "World Bank",
      TRUE ~ "Fuente detectada por Google"
    ),
    pais = NA_character_,
    region = NA_character_,
    ciudad = NA_character_,
    fecha_publicacion = as.Date(NA),
    fecha_cierre = as.Date(NA),
    dias_restantes = NA_integer_,
    monto = NA_real_,
    moneda = NA_character_,
    tipo_postulante = "no especificado",
    categoria = "resultado de busqueda",
    tematica = "sin clasificar",
    descripcion = normalize_text(items$snippet),
    enlace = normalize_text(items$link),
    estado = "por verificar",
    idioma = "no especificado",
    fecha_extraccion = Sys.time()
  ) |>
    standardize_licitaciones() |>
    ensure_links()
}
