get_ungm <- function(pages = as.integer(Sys.getenv("UNGM_PAGES", unset = "5"))) {
  fetch_page <- function(page_index) {
    body <- list(
      PageIndex = page_index,
      PageSize = 15,
      Title = "",
      Description = "",
      Reference = "",
      PublishedFrom = "",
      PublishedTo = "",
      DeadlineFrom = "",
      DeadlineTo = "",
      Countries = list(),
      Agencies = list(),
      UNSPSCs = list(),
      NoticeTypes = list(),
      SortField = "Deadline",
      SortAscending = TRUE,
      isPicker = FALSE,
      IsSustainable = FALSE,
      IsActive = TRUE,
      NoticeDisplayType = "",
      NoticeSearchTotalLabelId = "noticeSearchTotal",
      TypeOfCompetitions = list()
    )

    resp <- httr2::request("https://www.ungm.org/Public/Notice/Search") |>
      httr2::req_headers(
        "Content-Type" = "application/json",
        "X-Requested-With" = "XMLHttpRequest"
      ) |>
      httr2::req_body_json(body, auto_unbox = TRUE) |>
      httr2::req_timeout(30) |>
      httr2::req_perform()

    doc <- httr2::resp_body_string(resp) |>
      xml2::read_html()

    rows <- rvest::html_elements(doc, ".notice-table")
    if (!length(rows)) return(empty_licitaciones())

    purrr::map_dfr(rows, function(row) {
      cells <- rvest::html_elements(row, ".tableCell") |>
        rvest::html_text2() |>
        normalize_text()

      notice_id <- rvest::html_attr(row, "data-noticeid")
      href <- rvest::html_element(row, "a[href^='/Public/Notice/']") |>
        rvest::html_attr("href")

      title <- rvest::html_element(row, ".ungm-title") |>
        rvest::html_text2() |>
        normalize_text()

      tibble::tibble(
        id = normalize_text(cells[7] %||% notice_id),
        fuente = "UNGM",
        titulo = title,
        organismo = normalize_text(cells[5] %||% NA_character_),
        pais = normalize_text(cells[8] %||% NA_character_),
        region = NA_character_,
        ciudad = NA_character_,
        fecha_publicacion = parse_date_any(cells[4] %||% NA_character_),
        fecha_cierre = parse_date_any(cells[3] %||% NA_character_),
        dias_restantes = NA_integer_,
        monto = NA_real_,
        moneda = NA_character_,
        tipo_postulante = "no especificado",
        categoria = normalize_text(cells[6] %||% "procurement notice"),
        tematica = "sin clasificar",
        descripcion = paste(cells, collapse = " "),
        enlace = xml2::url_absolute(href %||% paste0("/Public/Notice/", notice_id), "https://www.ungm.org"),
        estado = "activa",
        idioma = "en",
        fecha_extraccion = Sys.time()
      )
    })
  }

  purrr::map_dfr(seq.int(0, max(0, pages - 1)), fetch_page) |>
    standardize_licitaciones()
}
