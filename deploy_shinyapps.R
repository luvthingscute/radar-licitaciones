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

rsconnect::deployApp(
  appDir = ".",
  appName = "monitor-licitaciones-internacionales",
  appTitle = "Monitor de Licitaciones Internacionales",
  forceUpdate = TRUE
)
