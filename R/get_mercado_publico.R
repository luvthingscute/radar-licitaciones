get_mercado_publico <- function(
    ticket = Sys.getenv("MERCADO_PUBLICO_TICKET", unset = "F8537A18-6766-4DEF-9E59-426B4FEE2844"),
    estado = "activas") {
  url <- "https://api.mercadopublico.cl/servicios/v1/publico/licitaciones.json"
  resp <- tryCatch(
    httr2::request(url) |>
      httr2::req_url_query(estado = estado, ticket = ticket) |>
      httr2::req_timeout(30) |>
      httr2::req_perform(),
    error = function(e) {
      fallback <- "data/mercado_publico_cache.rds"
      if (file.exists(fallback)) {
        message("Mercado Publico no disponible; usando cache de la fuente.")
        return(readRDS(fallback))
      }
      stop(e)
    }
  )

  if (is.data.frame(resp)) {
    return(resp)
  }

  raw <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  listado <- tibble::as_tibble(raw$Listado %||% list())

  if (!nrow(listado)) {
    return(empty_licitaciones())
  }

  datos <- tibble::tibble(
    id = normalize_text(col_or(listado, "CodigoExterno", col_or(listado, "Codigo"))),
    fuente = "Mercado Publico",
    titulo = normalize_text(col_or(listado, "Nombre")),
    organismo = normalize_text(col_or(listado, "NombreOrganismo")),
    pais = "Chile",
    region = NA_character_,
    ciudad = NA_character_,
    fecha_publicacion = parse_date_any(col_or(listado, "FechaPublicacion", col_or(listado, "FechaCreacion"))),
    fecha_cierre = parse_date_any(col_or(listado, "FechaCierre")),
    dias_restantes = NA_integer_,
    monto = clean_amount(col_or(listado, "MontoEstimado", NA_real_)),
    moneda = normalize_text(col_or(listado, "Moneda", "CLP")),
    tipo_postulante = "no especificado",
    categoria = normalize_text(col_or(listado, "CodigoTipo", "licitacion")),
    tematica = "sin clasificar",
    descripcion = normalize_text(col_or(listado, "Descripcion", col_or(listado, "Nombre"))),
    enlace = paste0(
      "https://www.mercadopublico.cl/Procurement/Modules/RFB/DetailsAcquisition.aspx?idlicitacion=",
      normalize_text(col_or(listado, "CodigoExterno", col_or(listado, "Codigo")))
    ),
    estado = normalize_text(col_or(listado, "Estado", "activa")),
    idioma = "es",
    fecha_extraccion = Sys.time()
  ) |>
    standardize_licitaciones()

  saveRDS(datos, "data/mercado_publico_cache.rds")
  datos
}
