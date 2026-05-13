get_cepal <- function() {
  pages <- list(
    "CEPAL - Expresiones de Interes" = list(
      url = "https://www.cepal.org/es/expresiones-interes",
      categoria = "expresion de interes"
    ),
    "CEPAL - Solicitudes de Informacion" = list(
      url = "https://www.cepal.org/es/solicitudes-informacion",
      categoria = "solicitud de informacion"
    )
  )

  purrr::imap_dfr(pages, function(meta, page_name) {
    page <- httr2::request(meta$url) |>
      httr2::req_headers("User-Agent" = "Mozilla/5.0") |>
      httr2::req_timeout(30) |>
      httr2::req_perform() |>
      httr2::resp_body_html()

    links <- rvest::html_elements(page, "a")
    href <- rvest::html_attr(links, "href")
    text <- rvest::html_text2(links) |> normalize_text()

    items <- tibble::tibble(text = text, href = href) |>
      dplyr::filter(
        !is.na(href),
        stringr::str_detect(text, stringr::regex("^(EOI|RFI)|UNECLAC|Procurement|Solicitud de", ignore_case = TRUE)) |
          stringr::str_detect(href, stringr::regex("eoi|rfi|solicitud|procurement", ignore_case = TRUE))
      ) |>
      dplyr::filter(!stringr::str_detect(text, stringr::regex("Registro de Proveedores|UN Procurement Division", ignore_case = TRUE))) |>
      dplyr::mutate(enlace = xml2::url_absolute(href, meta$url)) |>
      dplyr::distinct(text, enlace, .keep_all = TRUE)

    if (!nrow(items)) return(empty_licitaciones())

    tibble::tibble(
      id = purrr::map_chr(items$text, ~ stringr::str_extract(.x, "(EOI|RFI)[A-Z0-9 -]*\\d{3,}") %||% digest::digest(.x, algo = "xxhash32")),
      fuente = "CEPAL",
      titulo = items$text,
      organismo = "CEPAL / Naciones Unidas",
      pais = "Chile",
      region = "America Latina y el Caribe",
      ciudad = "Santiago",
      fecha_publicacion = as.Date(NA),
      fecha_cierre = as.Date(NA),
      dias_restantes = NA_integer_,
      monto = NA_real_,
      moneda = NA_character_,
      tipo_postulante = "no especificado",
      categoria = meta$categoria,
      tematica = "sin clasificar",
      descripcion = paste(page_name, items$text),
      enlace = items$enlace,
      estado = "publicada",
      idioma = "es",
      fecha_extraccion = Sys.time()
    ) |>
      standardize_licitaciones()
  })
}
