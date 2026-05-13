required_packages <- c(
  "shiny", "bslib", "DT", "dplyr", "lubridate", "stringr", "httr2",
  "jsonlite", "rvest", "purrr", "echarts4r", "tibble", "xml2", "digest"
)

missing_packages <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing_packages)) {
  stop(
    "Faltan paquetes: ",
    paste(missing_packages, collapse = ", "),
    "\nInstalalos con install.packages(c(",
    paste(sprintf("'%s'", missing_packages), collapse = ", "),
    "))",
    call. = FALSE
  )
}

invisible(lapply(required_packages, library, character.only = TRUE))

r_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
invisible(lapply(r_files, source, local = FALSE))
