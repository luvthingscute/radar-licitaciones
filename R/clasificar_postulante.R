clasificar_postulante <- function(titulo, descripcion = "") {
  texto <- stringr::str_to_lower(paste(titulo, descripcion))

  dplyr::case_when(
    stringr::str_detect(texto, "consultor individual|individual consultant|persona natural") ~ "consultor individual",
    stringr::str_detect(texto, "ong|ngo|organizacion no gubernamental|non-governmental") ~ "ONG",
    stringr::str_detect(texto, "universidad|university|academia|research center") ~ "universidad",
    stringr::str_detect(texto, "gobierno|government|ministry|municipalidad|public agency") ~ "gobierno",
    stringr::str_detect(texto, "persona juridica|legal entity") ~ "persona juridica",
    stringr::str_detect(texto, "empresa|firma|company|supplier|vendor|contratista") ~ "empresa",
    TRUE ~ "no especificado"
  )
}
