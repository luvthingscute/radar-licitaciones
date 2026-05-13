`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

args <- commandArgs(FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
app_dir <- if (length(file_arg)) {
  dirname(normalizePath(sub("^--file=", "", file_arg[[1]])))
} else {
  getwd()
}

setwd(app_dir)

shiny::runApp(
  ".",
  host = "127.0.0.1",
  port = 3838,
  launch.browser = FALSE
)
