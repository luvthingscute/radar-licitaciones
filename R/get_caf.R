get_caf <- function() {
  urls <- c(
    "https://www.caf.com/es/trabaja-con-nosotros/convocatorias/",
    "https://www.caf.com/es/trabaja-con-nosotros/licitaciones/"
  )

  purrr::map_dfr(urls, function(url) {
    resp <- httr2::request(url) |>
      httr2::req_headers("User-Agent" = "Mozilla/5.0") |>
      httr2::req_timeout(30) |>
      httr2::req_perform()

    html <- httr2::resp_body_string(resp)
    if (stringr::str_detect(html, "Incapsula|Request unsuccessful")) {
      message("CAF bloquea scraping automatico desde esta red. Conector preparado; revisar portal oficial.")
      return(empty_licitaciones())
    }

    page <- xml2::read_html(html)
    text <- rvest::html_text2(page)
    lines <- stringr::str_split(text, "\\r?\\n") |>
      purrr::pluck(1) |>
      normalize_text()
    lines <- lines[nzchar(lines)]

    starts <- which(stringr::str_detect(lines, stringr::regex("Convocatoria|Licitacion|Licitación|Consultoria|Consultoría|Concurso", ignore_case = TRUE)))
    starts <- starts[!stringr::str_detect(lines[starts], stringr::regex("Filtrar|Trabajar en CAF|Ver mas|Ver más", ignore_case = TRUE))]
    if (!length(starts)) return(empty_licitaciones())

    purrr::map_dfr(head(starts, 50), function(i) {
      block <- lines[i:min(length(lines), i + 8)]
      cierre_line <- block[stringr::str_detect(block, stringr::regex("^Cierre:", ignore_case = TRUE))][1] %||% NA_character_
      estado <- block[stringr::str_detect(block, stringr::regex("Convocatoria abierta|Convocatoria cerrada|Abierto|Cerrado", ignore_case = TRUE))][1] %||% "publicada"

      tibble::tibble(
        id = digest::digest(paste(block, collapse = "|"), algo = "xxhash32"),
        fuente = "CAF",
        titulo = block[[1]],
        organismo = "CAF - Banco de Desarrollo de America Latina y el Caribe",
        pais = NA_character_,
        region = "America Latina y el Caribe",
        ciudad = NA_character_,
        fecha_publicacion = as.Date(NA),
        fecha_cierre = parse_date_any(cierre_line),
        dias_restantes = NA_integer_,
        monto = NA_real_,
        moneda = NA_character_,
        tipo_postulante = "no especificado",
        categoria = ifelse(stringr::str_detect(url, "licitaciones"), "licitacion", "convocatoria"),
        tematica = "sin clasificar",
        descripcion = paste(block, collapse = " "),
        enlace = url,
        estado = estado,
        idioma = "es",
        fecha_extraccion = Sys.time()
      )
    }) |>
      standardize_licitaciones()
  })
}
