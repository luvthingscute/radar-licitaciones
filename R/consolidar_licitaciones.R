consolidar_licitaciones <- function(use_cache = FALSE, cache_path = "data/licitaciones_cache.rds") {
  dir.create(dirname(cache_path), showWarnings = FALSE, recursive = TRUE)

  if (use_cache && file.exists(cache_path)) {
    return(readRDS(cache_path))
  }

  old_cache <- if (file.exists(cache_path)) readRDS(cache_path) else empty_licitaciones()

  datos <- dplyr::bind_rows(
    safe_source(get_mercado_publico(), "Mercado Publico"),
    safe_source(get_pnud(), "PNUD / UNDP"),
    safe_source(get_ungm(), "UNGM"),
    safe_source(get_worldbank(), "World Bank Procurement"),
    safe_source(get_bid(), "BID"),
    safe_source(get_cepal(), "CEPAL")
  ) |>
    standardize_licitaciones() |>
    dplyr::mutate(
      titulo = normalize_text(titulo),
      descripcion = normalize_text(descripcion),
      pais = dplyr::na_if(normalize_text(pais), ""),
      organismo = dplyr::na_if(normalize_text(organismo), ""),
      dias_restantes = calculate_days_remaining(fecha_cierre),
      estado = dplyr::case_when(
        is.na(fecha_cierre) ~ dplyr::coalesce(estado, "sin fecha"),
        fecha_cierre >= Sys.Date() ~ "activa",
        TRUE ~ "cerrada"
      ),
      tematica = clasificar_tematica(titulo, descripcion),
      tipo_postulante = clasificar_postulante(titulo, descripcion)
    ) |>
    improve_amounts() |>
    ensure_links() |>
    dplyr::filter(fuente %in% ALL_SOURCES) |>
    dplyr::distinct(fuente, id, titulo, fecha_cierre, .keep_all = TRUE)

  if (nrow(old_cache)) {
    missing_sources <- intersect(setdiff(unique(old_cache$fuente), unique(datos$fuente)), ALL_SOURCES)
    if (length(missing_sources)) {
      message(
        "Se conservan fuentes del cache anterior porque fallaron en esta extraccion: ",
        paste(missing_sources, collapse = ", ")
      )
      datos <- dplyr::bind_rows(
        datos,
        dplyr::filter(old_cache, fuente %in% missing_sources)
      ) |>
        dplyr::distinct(fuente, id, titulo, fecha_cierre, .keep_all = TRUE)
    }
  }

  if (file.exists(cache_path)) {
    backup_path <- sub("\\.rds$", "_backup.rds", cache_path)
    file.copy(cache_path, backup_path, overwrite = TRUE)
  }

  saveRDS(datos, cache_path)
  datos
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}
