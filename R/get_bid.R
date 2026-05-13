get_bid <- function() {
  # BID publica las oportunidades de adquisiciones para proyectos mediante
  # la app BID for the Americas. La pagina oficial no expone una tabla HTML
  # de avisos ni una API publica documentada para scraping directo.
  #
  # Si se habilita un endpoint/API exportable, conectar aqui manteniendo
  # STANDARD_COLUMNS. Por ahora se devuelve cero filas para no inventar
  # oportunidades ni apuntar busquedas genericas como licitaciones reales.
  message("BID integrado como conector. Sin endpoint publico estructurado disponible; usar portal BID for the Americas.")
  empty_licitaciones()
}
