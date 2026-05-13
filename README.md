# Monitor de Licitaciones Internacionales

Dashboard profesional en R Shiny para monitorear oportunidades de licitacion desde Mercado Publico, PNUD/UNDP, UNGM y World Bank Procurement.

## Estructura

```text
licitaciones_dashboard/
├── app.R
├── global.R
├── data/
├── R/
│   ├── get_mercado_publico.R
│   ├── get_pnud.R
│   ├── get_ungm.R
│   ├── get_worldbank.R
│   ├── get_bid.R
│   ├── get_cepal.R
│   ├── get_caf.R
│   ├── get_google_search.R
│   ├── consolidar_licitaciones.R
│   ├── clasificar_tematica.R
│   ├── clasificar_postulante.R
│   └── utils.R
├── www/
│   └── styles.css
└── README.md
```

## Instalacion

```r
install.packages(c(
  "shiny", "bslib", "DT", "dplyr", "lubridate", "stringr", "httr2",
  "jsonlite", "rvest", "purrr", "echarts4r", "tibble", "xml2", "digest"
))
```

## Ejecucion

```r
shiny::runApp("licitaciones_dashboard")
```

## Publicacion

Para publicar en shinyapps.io, configura tu cuenta localmente con `rsconnect::setAccountInfo()` en la consola de R/RStudio y luego ejecuta:

```r
source("deploy_shinyapps.R")
```

No subas tokens, secrets, `.Renviron`, `rsconnect/`, caches ni logs al repositorio.

## Mercado Publico

La API de Mercado Publico requiere un ticket. Configuralo antes de ejecutar:

```r
Sys.setenv(MERCADO_PUBLICO_TICKET = "TU_TICKET")
```

La documentacion oficial indica que el endpoint de licitaciones acepta consultas por fecha, codigo y estado, incluyendo `estado=activas`.

## Google Search

La fuente `get_google_search()` usa Google Custom Search JSON API si existen estas variables:

```r
Sys.setenv(GOOGLE_SEARCH_API_KEY = "TU_API_KEY")
Sys.setenv(GOOGLE_SEARCH_CX = "TU_SEARCH_ENGINE_ID")
```

La consulta se puede ajustar con `GOOGLE_SEARCH_QUERY`. Todos los registros quedan con enlace; si una fuente no entrega URL, el sistema crea un enlace de busqueda de respaldo.

## Fuentes internacionales

Los conectores de PNUD, UNGM y World Bank estan encapsulados. En esta primera version:

- `get_pnud()` intenta leer el portal publico y vuelve a datos demo si el HTML cambia.
- `get_ungm()` queda como placeholder funcional porque UNGM depende de busqueda web y endpoints internos no garantizados.
- `get_worldbank()` queda como placeholder funcional hasta definir el endpoint oficial mas adecuado.

## BID, CEPAL y CAF

- `get_bid()` queda integrado como conector del motor. BID publica las oportunidades por la app BID for the Americas; si se obtiene un endpoint exportable/documentado, se conecta ahi.
- `get_cepal()` extrae oportunidades reales desde paginas oficiales de Expresiones de Interes y Solicitudes de Informacion.
- `get_caf()` intenta leer Convocatorias y Licitaciones oficiales; si el sitio bloquea scraping automatico, devuelve cero filas sin inventar datos.

El dashboard sigue funcionando aunque una fuente falle gracias a `tryCatch()` en `consolidar_licitaciones()`.

## Preparado para IA

La estructura estandar permite agregar despues:

- embeddings y matching semantico
- score de pertinencia por perfil de empresa
- alertas automaticas
- mapas y analisis geografico
- extraccion de PDFs/TDR
- historial y seguimiento de adjudicaciones
