source("global.R")

message("Iniciando actualizacion programada de licitaciones...")
datos <- consolidar_licitaciones(use_cache = FALSE)

message("Registros actualizados: ", nrow(datos))
message("Fuentes con datos: ", paste(sort(unique(datos$fuente)), collapse = ", "))
message("Cache guardado en data/licitaciones_cache.rds")
