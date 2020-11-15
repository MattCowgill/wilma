#' @import shiny
wilma <- function(...) {

  ui <- fluidPage(
    fluidRow(
        uiOutput("series_select")
        ),
    fluidRow(
      column(
        width = 12,
        girafeOutput("plot",
                     width = '100%',
                     height = '600px')
      )
    )
  )

  server <- function(input, output, session) {

    output$series_select <- renderUI({
      selectInput("series_select",
                  label = "Select series",
                  choices = as.character(available_data$series),
                  selected = as.character(available_data$series[available_data$series_id == "A83895396W"]),
                  width = '100%')
    })

    chart_data <- reactive({
      shiny::req(input$series_select)
      selected_series <- input$series_select
      selected_id <- as.character(available_data$series_id[available_data$series == selected_series])
      chart_data <- data[[selected_id]]
    })

    output$plot <- renderGirafe({
      static_plot <- make_graph(chart_data())
      ggiraph::girafe(ggobj = static_plot, width_svg = 9, height_svg = 5)
    })



  }

  shinyApp(ui, server)
}
