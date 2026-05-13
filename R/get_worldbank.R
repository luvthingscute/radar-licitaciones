get_worldbank <- function(rows = as.integer(Sys.getenv("WORLDBANK_ROWS", unset = "200"))) {
  year_query <- format(Sys.Date(), "%Y")

  resp <- httr2::request("http://search.worldbank.org/api/procnotices") |>
    httr2::req_url_query(format = "json", rows = rows, qterm = year_query) |>
    httr2::req_timeout(30) |>
    httr2::req_perform()

  raw <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  notices <- tibble::as_tibble(raw$procnotices %||% list())

  if (!nrow(notices)) {
    return(empty_licitaciones())
  }

  notices |>
    dplyr::filter(!stringr::str_detect(notice_type %||% "", "Contract Award")) |>
    dplyr::transmute(
      id = normalize_text(id),
      fuente = "World Bank Procurement",
      titulo = normalize_text(bid_description %||% project_name),
      organismo = normalize_text(contact_organization %||% "World Bank"),
      pais = normalize_text(project_ctry_name),
      region = NA_character_,
      ciudad = NA_character_,
      fecha_publicacion = parse_date_any(noticedate %||% submission_date),
      fecha_cierre = as.Date(NA),
      dias_restantes = NA_integer_,
      monto = NA_real_,
      moneda = NA_character_,
      tipo_postulante = "no especificado",
      categoria = normalize_text(notice_type),
      tematica = "sin clasificar",
      descripcion = normalize_text(gsub("<[^>]+>", " ", notice_text %||% bid_description)),
      enlace = paste0("https://projects.worldbank.org/en/projects-operations/procurement?id=", id),
      estado = normalize_text(notice_status %||% "publicada"),
      idioma = normalize_text(notice_lang_name %||% "en"),
      fecha_extraccion = Sys.time()
    ) |>
    standardize_licitaciones()
}
