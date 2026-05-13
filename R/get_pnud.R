get_pnud <- function() {
  url <- "https://procurement-notices.undp.org/search.cfm"

  page <- httr2::request(url) |>
    httr2::req_timeout(30) |>
    httr2::req_perform() |>
    httr2::resp_body_html()

  lines <- page |>
    rvest::html_text2() |>
    stringr::str_split("\\r?\\n") |>
    purrr::pluck(1) |>
    stringr::str_squish()
  lines <- lines[nzchar(lines)]

  starts <- which(lines == "Title" & dplyr::lead(lines, 2) == "Ref No")
  if (!length(starts)) return(empty_licitaciones())

  links <- page |>
    rvest::html_elements("a") |>
    rvest::html_attr("href") |>
    na.omit() |>
    as.character()
  notice_links <- links[stringr::str_detect(links, "view_negotiation\\.cfm\\?nego_id=")]
  notice_links <- xml2::url_absolute(notice_links, url)

  parsed <- purrr::map2_dfr(starts, seq_along(starts), function(i, idx) {
    block <- lines[i:min(length(lines), i + 15)]
    value_after <- function(label) {
      pos <- which(block == label)
      if (length(pos) && length(block) >= pos[[1]] + 1) block[[pos[[1]] + 1]] else NA_character_
    }

    office_country <- value_after("UNDP Office/Country")
    pais <- stringr::str_split_fixed(office_country %||% "", "/", 2)[, 2]

    tibble::tibble(
      id = value_after("Ref No") %||% digest::digest(block, algo = "xxhash32"),
      fuente = "PNUD / UNDP",
      titulo = value_after("Title"),
      organismo = stringr::str_split_fixed(office_country %||% "", "/", 2)[, 1],
      pais = stringr::str_to_title(pais),
      region = NA_character_,
      ciudad = NA_character_,
      fecha_publicacion = parse_date_any(value_after("Posted")),
      fecha_cierre = parse_date_any(value_after("Deadline")),
      dias_restantes = NA_integer_,
      monto = NA_real_,
      moneda = NA_character_,
      tipo_postulante = "no especificado",
      categoria = value_after("Process") %||% "procurement notice",
      tematica = "sin clasificar",
      descripcion = paste(block, collapse = " "),
      enlace = if (idx <= length(notice_links)) notice_links[[idx]] else NA_character_,
      estado = "activa",
      idioma = "en",
      fecha_extraccion = Sys.time()
    )
  })

  standardize_licitaciones(parsed)
}
