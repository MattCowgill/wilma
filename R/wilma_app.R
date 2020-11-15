#' @import shiny
wilma <- function(...) {

  ui <- fluidPage(
    fluidRow(
        uiOutput("series_select")
        ),
    fluidRow(
      uiOutput("date_select")
    ),
    fluidRow(
      column(
        width = 12,
        ggiraph::girafeOutput("plot",
                     width = '100%',
                     height = '600px')
      )
    ),
    fluidRow(
      title = "How to get this data in R:",
      verbatimTextOutput("code")
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

    selected_id <- reactive({
      shiny::req(input$series_select)
      selected_series <- input$series_select
      as.character(available_data$series_id[available_data$series == selected_series])
    })

    chart_data <- reactive({
      chart_data <- data[[selected_id()]]
    })

    output$date_select <- renderUI({
      req(chart_data())
      min_data_date <- min(chart_data()$date)
      max_data_date <- max(chart_data()$date)
      sliderInput("date_select",
                  label = "Select date range",
                  min = min_data_date,
                  max = max_data_date,
                  value = c(min_data_date, max_data_date))
    })

    output$plot <- ggiraph::renderGirafe({
      req(chart_data())
      req(input$date_select)
      static_plot <- make_graph(chart_data = chart_data(),
                                start_date = input$date_select[1],
                                end_date = input$date_select[2])

      ggiraph::girafe(ggobj = static_plot, width_svg = 9, height_svg = 5)
    })


    output$code <- renderText({
      paste0("readabs::read_abs(series_id = ", selected_id(), ")")
    })

  }

  shinyApp(ui, server)
}
