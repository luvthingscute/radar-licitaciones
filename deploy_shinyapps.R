# Ejecuta este script localmente para publicar en shinyapps.io.
# No guardes tokens ni secrets en el repositorio.

if (!requireNamespace("rsconnect", quietly = TRUE)) {
  install.packages("rsconnect")
}

# Configura la cuenta una sola vez en tu consola local, no en GitHub:
# rsconnect::setAccountInfo(
#   name = "TU_USUARIO",
#   token = "TU_TOKEN",
#   secret = "TU_SECRET"
# )

if (nzchar(Sys.getenv("SHINYAPPS_TOKEN")) && nzchar(Sys.getenv("SHINYAPPS_SECRET"))) {
  rsconnect::setAccountInfo(
    name = Sys.getenv("SHINYAPPS_ACCOUNT", unset = "florenciamunozobon"),
    token = Sys.getenv("SHINYAPPS_TOKEN"),
    secret = Sys.getenv("SHINYAPPS_SECRET")
  )
}

app_files <- c(
  "app.R",
  "global.R",
  list.files("R", pattern = "\\.R$", full.names = TRUE),
  list.files("www", full.names = TRUE),
  "README.md",
  "data/licitaciones_cache.rds",
  "data/mercado_publico_cache.rds"
)

app_files <- app_files[file.exists(app_files)]

rsconnect::deployApp(
  appDir = ".",
  appName = "monitor-licitaciones-internacionales",
  appTitle = "Monitor de Licitaciones Internacionales",
  appFiles = app_files,
  forceUpdate = TRUE
)
