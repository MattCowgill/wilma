#' @import shiny
wilma <- function(...) {

  ui <- fluidPage(
    fluidRow(
        uiOutput("series_select")
        ),
    fluidRow(
      plotOutput("plot")
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
      selected_series <- input$series_select
      print(selected_series)
      selected_id <- as.character(available_data$series_id[available_data$series == selected_series])
      chart_data <- data[[selected_id]]
    })

    reactive({print(chart_data())})

    output$plot <- renderPlot({
      chart_data() %>%
        make_graph()
    })



  }

  shinyApp(ui, server)
}
