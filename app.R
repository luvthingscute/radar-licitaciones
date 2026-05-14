source("global.R")

theme <- bslib::bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#2563eb",
  secondary = "#475569",
  success = "#059669",
  danger = "#dc2626",
  base_font = bslib::font_google("Work Sans", local = FALSE),
  heading_font = bslib::font_google("Work Sans", local = FALSE)
)

ui <- bslib::page_sidebar(
  title = "Monitor de Licitaciones Internacionales",
  theme = theme,
  fillable = TRUE,
  sidebar = bslib::sidebar(
    width = 320,
    class = "app-sidebar",
    actionButton("refresh", "Actualizar datos", class = "btn-primary w-100"),
    hr(),
    selectizeInput("fuente", "Fuente", choices = NULL, multiple = TRUE),
    selectizeInput("pais", "Pais", choices = NULL, multiple = TRUE),
    selectizeInput("tematica", "Tematica", choices = NULL, multiple = TRUE),
    selectizeInput("postulante", "Tipo de postulante", choices = NULL, multiple = TRUE),
    checkboxInput("usar_fecha", "Filtrar por fecha de cierre", value = FALSE),
    dateRangeInput("fecha_cierre", "Fecha de cierre", start = Sys.Date(), end = Sys.Date() + 90),
    sliderInput("monto", "Monto", min = 0, max = 1000000, value = c(0, 1000000), step = 1000),
    textInput("keyword", "Palabra clave", placeholder = "Buscar en titulo u organismo")
  ),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
  div(
    class = "dashboard-shell",
    div(
      class = "kpi-grid",
      uiOutput("kpi_total"),
      uiOutput("kpi_activas"),
      uiOutput("kpi_proximas"),
      uiOutput("kpi_fuentes"),
      uiOutput("kpi_paises")
    ),
    div(class = "data-status", textOutput("estado_datos")),
    bslib::navset_card_tab(
      full_screen = TRUE,
      bslib::nav_panel(
        "Explorador",
        div(
          class = "table-toolbar",
          div(textOutput("favoritos_estado")),
          div(
            class = "table-actions",
            downloadButton("descargar_base_csv", "Base CSV", class = "btn-outline-primary"),
            downloadButton("descargar_favoritos", "Favoritos CSV", class = "btn-primary")
          )
        ),
        DT::DTOutput("tabla")
      ),
      bslib::nav_panel(
        "Fuentes",
        DT::DTOutput("tabla_fuentes")
      ),
      bslib::nav_panel(
        "Visualizaciones",
        div(
          class = "chart-grid",
          bslib::card(bslib::card_header("Licitaciones por fuente"), echarts4r::echarts4rOutput("chart_fuente", height = "310px")),
          bslib::card(bslib::card_header("Licitaciones por pais"), echarts4r::echarts4rOutput("chart_pais", height = "310px")),
          bslib::card(bslib::card_header("Licitaciones por tematica"), echarts4r::echarts4rOutput("chart_tematica", height = "310px")),
          bslib::card(bslib::card_header("Cierres por fecha"), echarts4r::echarts4rOutput("chart_cierres", height = "310px")),
          bslib::card(class = "wide-card", bslib::card_header("Evolucion temporal"), echarts4r::echarts4rOutput("chart_evolucion", height = "320px"))
        )
      )
    )
  )
)

server <- function(input, output, session) {
  licitaciones <- reactiveVal(consolidar_licitaciones(use_cache = TRUE))

  observeEvent(input$refresh, {
    showNotification("Actualizando fuentes en vivo. Puede tardar unos minutos.", type = "message", duration = 6)
    datos <- withProgress(message = "Actualizando licitaciones", value = 0.1, {
      incProgress(0.2, detail = "Consultando fuentes externas")
      nuevos_datos <- consolidar_licitaciones(use_cache = FALSE)
      incProgress(0.7, detail = "Actualizando dashboard")
      nuevos_datos
    })
    licitaciones(datos)
    showNotification("Datos actualizados.", type = "message", duration = 5)
  })

  observe({
    df <- licitaciones()
    updateSelectizeInput(session, "fuente", choices = sort(unique(c(ALL_SOURCES, na.omit(df$fuente)))), server = TRUE)
    updateSelectizeInput(session, "pais", choices = sort(unique(na.omit(df$pais))), server = TRUE)
    updateSelectizeInput(session, "tematica", choices = sort(unique(na.omit(df$tematica))), server = TRUE)
    updateSelectizeInput(session, "postulante", choices = sort(unique(na.omit(df$tipo_postulante))), server = TRUE)

    monto_max <- max(df$monto, na.rm = TRUE)
    if (is.finite(monto_max) && monto_max > 0) {
      updateSliderInput(session, "monto", max = ceiling(monto_max), value = c(0, ceiling(monto_max)))
    }
  })

  filtradas <- reactive({
    df <- licitaciones()

    if (length(input$fuente)) df <- dplyr::filter(df, fuente %in% input$fuente)
    if (length(input$pais)) df <- dplyr::filter(df, pais %in% input$pais)
    if (length(input$tematica)) df <- dplyr::filter(df, tematica %in% input$tematica)
    if (length(input$postulante)) df <- dplyr::filter(df, tipo_postulante %in% input$postulante)

    if (isTRUE(input$usar_fecha) && !is.null(input$fecha_cierre) && all(!is.na(input$fecha_cierre))) {
      df <- dplyr::filter(df, is.na(fecha_cierre) | dplyr::between(fecha_cierre, input$fecha_cierre[1], input$fecha_cierre[2]))
    }

    if (!is.null(input$monto)) {
      df <- dplyr::filter(df, is.na(monto) | dplyr::between(monto, input$monto[1], input$monto[2]))
    }

    if (nzchar(input$keyword %||% "")) {
      patron <- stringr::regex(input$keyword, ignore_case = TRUE)
      df <- dplyr::filter(df, stringr::str_detect(paste(titulo, organismo, descripcion), patron))
    }

    df
  })

  kpi_card <- function(label, value, accent = "blue") {
    div(class = paste("kpi-card", accent), span(label), strong(value))
  }

  output$kpi_total <- renderUI(kpi_card("Total licitaciones", nrow(filtradas())))
  output$kpi_activas <- renderUI(kpi_card("Activas", sum(filtradas()$estado == "activa", na.rm = TRUE), "green"))
  output$kpi_proximas <- renderUI(kpi_card("Cierres < 7 dias", sum(filtradas()$dias_restantes >= 0 & filtradas()$dias_restantes < 7, na.rm = TRUE), "amber"))
  output$kpi_fuentes <- renderUI(kpi_card("Fuentes monitoreadas", length(ALL_SOURCES)))
  output$kpi_paises <- renderUI(kpi_card("Paises", dplyr::n_distinct(filtradas()$pais, na.rm = TRUE)))

  output$estado_datos <- renderText({
    df <- licitaciones()
    if (!nrow(df)) {
      return("Datos cargados: 0 registros. Presiona Actualizar datos o revisa la conexion.")
    }

    paste0(
      "Datos cargados: ", format(nrow(df), big.mark = ".", decimal.mark = ","),
      " registros | Fuentes con datos: ", paste(sort(unique(df$fuente)), collapse = ", "),
      " | Ultima extraccion: ", format(max(df$fecha_extraccion, na.rm = TRUE), "%Y-%m-%d %H:%M")
    )
  })

  output$tabla_fuentes <- DT::renderDT({
    conteos <- licitaciones() |>
      dplyr::count(fuente, name = "registros")

    df <- SOURCE_REGISTRY |>
      dplyr::left_join(conteos, by = "fuente") |>
      dplyr::mutate(
        registros = dplyr::coalesce(registros, 0L),
        estado = dplyr::if_else(registros > 0, "Con datos", "Sin registros extraidos"),
        portal = sprintf('<a href="%s" target="_blank" rel="noopener noreferrer">Abrir portal</a>', portal)
      ) |>
      dplyr::select(fuente, estado, registros, modo, nota, portal)

    DT::datatable(
      df,
      escape = FALSE,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        dom = "tip",
        scrollX = TRUE,
        language = list(
          info = "Mostrando _START_ a _END_ de _TOTAL_ fuentes",
          zeroRecords = "No se encontraron fuentes",
          emptyTable = "No hay fuentes configuradas",
          paginate = list(previous = "Anterior", `next` = "Siguiente")
        )
      )
    )
  }, server = FALSE)

  output$tabla <- DT::renderDT({
    df <- filtradas() |>
      dplyr::transmute(
        fuente,
        titulo,
        organismo,
        pais,
        fecha_publicacion,
        fecha_cierre,
        monto = dplyr::if_else(
          is.na(monto),
          NA_character_,
          stringr::str_squish(paste(
            dplyr::coalesce(moneda, ""),
            format(round(monto, 0), big.mark = ".", decimal.mark = ",")
          ))
        ),
        tematica,
        tipo_postulante,
        portal = sprintf('<a href="%s" target="_blank" rel="noopener noreferrer">Portal</a>', source_portal_url(fuente)),
        enlace = dplyr::if_else(
          is.na(enlace) | enlace == "",
          "",
          sprintf('<a href="%s" target="_blank" rel="noopener noreferrer">Buscar</a>', enlace)
        )
      )

    shiny::validate(
      shiny::need(nrow(df) > 0, "No hay licitaciones para los filtros seleccionados.")
    )

    DT::datatable(
      df,
      escape = FALSE,
      rownames = FALSE,
      extensions = "Buttons",
      selection = "multiple",
      options = list(
        dom = "Bfrtip",
        buttons = c("copy", "csv"),
        pageLength = 25,
        deferRender = TRUE,
        processing = TRUE,
        scrollX = TRUE,
        language = list(
          processing = "Procesando...",
          search = "Buscar:",
          lengthMenu = "Mostrar _MENU_ registros",
          info = "Mostrando _START_ a _END_ de _TOTAL_ registros",
          infoEmpty = "Mostrando 0 registros",
          infoFiltered = "(filtrado de _MAX_ registros totales)",
          loadingRecords = "Cargando...",
          zeroRecords = "No se encontraron resultados",
          emptyTable = "No hay datos disponibles",
          paginate = list(
            first = "Primero",
            previous = "Anterior",
            `next` = "Siguiente",
            last = "Ultimo"
          )
        )
      )
    )
  }, server = TRUE)

  favoritos <- reactive({
    seleccion <- input$tabla_rows_selected
    df <- filtradas()

    if (is.null(seleccion) || !length(seleccion) || !nrow(df)) {
      return(df[0, , drop = FALSE])
    }

    seleccion <- seleccion[seleccion <= nrow(df)]
    df[seleccion, , drop = FALSE]
  })

  output$favoritos_estado <- renderText({
    n <- nrow(favoritos())
    if (n == 1) {
      "1 favorito seleccionado"
    } else {
      paste(n, "favoritos seleccionados")
    }
  })

  output$descargar_favoritos <- downloadHandler(
    filename = function() {
      paste0("favoritos_licitaciones_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      utils::write.csv(favoritos(), file, row.names = FALSE, na = "", fileEncoding = "UTF-8")
    }
  )

  output$descargar_base_csv <- downloadHandler(
    filename = function() {
      paste0("licitaciones_filtradas_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      utils::write.csv(filtradas(), file, row.names = FALSE, na = "", fileEncoding = "UTF-8")
    }
  )

  output$chart_fuente <- echarts4r::renderEcharts4r({
    filtradas() |> dplyr::count(fuente, name = "n") |> echarts4r::e_charts(fuente) |> echarts4r::e_bar(n) |> echarts4r::e_tooltip()
  })

  output$chart_pais <- echarts4r::renderEcharts4r({
    filtradas() |> dplyr::count(pais, name = "n", sort = TRUE) |> dplyr::slice_head(n = 12) |> echarts4r::e_charts(pais) |> echarts4r::e_bar(n) |> echarts4r::e_flip_coords() |> echarts4r::e_tooltip()
  })

  output$chart_tematica <- echarts4r::renderEcharts4r({
    filtradas() |> dplyr::count(tematica, name = "n", sort = TRUE) |> dplyr::slice_head(n = 12) |> echarts4r::e_charts(tematica) |> echarts4r::e_bar(n) |> echarts4r::e_flip_coords() |> echarts4r::e_tooltip()
  })

  output$chart_cierres <- echarts4r::renderEcharts4r({
    filtradas() |> dplyr::filter(!is.na(fecha_cierre)) |> dplyr::count(fecha_cierre, name = "n") |> echarts4r::e_charts(fecha_cierre) |> echarts4r::e_line(n, smooth = TRUE) |> echarts4r::e_tooltip()
  })

  output$chart_evolucion <- echarts4r::renderEcharts4r({
    filtradas() |> dplyr::filter(!is.na(fecha_publicacion)) |> dplyr::count(fecha_publicacion, fuente, name = "n") |> echarts4r::e_charts(fecha_publicacion) |> echarts4r::e_line(n) |> echarts4r::e_tooltip(trigger = "axis")
  })
}

shinyApp(ui, server)
