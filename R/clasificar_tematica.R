clasificar_tematica <- function(titulo, descripcion = "") {
  texto <- stringr::str_to_lower(paste(titulo, descripcion))

  reglas <- list(
    "medio ambiente" = c("medio ambiente", "ambiental", "biodiversidad", "residuo"),
    "cambio climatico" = c("cambio climatico", "clima", "climatico", "adaptacion", "mitigacion"),
    "desarrollo urbano" = c("urbano", "ciudad", "territorial", "plan regulador"),
    "vivienda" = c("vivienda", "habitacional", "housing"),
    "energia" = c("energia", "energetica", "renovable", "solar", "eolica"),
    "transicion justa" = c("transicion justa", "just transition"),
    "transporte" = c("transporte", "movilidad", "metro", "bus", "logistica"),
    "agua" = c("agua", "hidrico", "saneamiento", "alcantarillado"),
    "mineria" = c("mineria", "minero", "minerales", "cobre", "lithium", "litio"),
    "gobernanza" = c("gobernanza", "institucional", "transparencia", "politica publica"),
    "datos / GIS" = c("gis", "sig", "geoespacial", "data", "datos", "dashboard", "analytics"),
    "infraestructura" = c("infraestructura", "obra", "construccion", "facility"),
    "consultoria" = c("consultoria", "consultancy", "consultant", "asesoria", "asistencia tecnica"),
    "salud" = c("salud", "hospital", "sanitario", "health"),
    "educacion" = c("educacion", "capacitacion", "training", "escuela", "universidad"),
    "agricultura" = c("agricultura", "agricola", "rural", "forestal", "food"),
    "tecnologia" = c("tecnologia", "software", "sistema", "plataforma", "digital", "it")
  )

  purrr::map_chr(texto, function(x) {
    hit <- purrr::keep(names(reglas), function(cat) {
      any(stringr::str_detect(x, stringr::fixed(reglas[[cat]], ignore_case = TRUE)))
    })
    if (length(hit)) hit[[1]] else "sin clasificar"
  })
}
