#' @import shiny
wilma <- function(...) {
  ui <- navbarPage(
    theme = shinythemes::shinytheme("paper"),
    title = "wilma",
    tabPanel(
      "Use wilma",
      uiOutput("collection_select"),
      uiOutput("series_select"),
      shinyWidgets::chooseSliderSkin("Flat",
                                     color = "#F8766D"),
      uiOutput("date_select"),
      ggiraph::girafeOutput("plot",
        width = "100%",
        height = "600px"
      ),
      downloadButton("download_data", "Download data"),
      div("Get this data in R"),
      verbatimTextOutput("code")
    ),
    tabPanel(
      "About Wilma",
      includeMarkdown("www/about.md")
    )
  )

  server <- function(input, output, session) {
    output$collection_select <- renderUI({
      selectInput("collection_select",
        label = "Select data collection",
        choices = unique(available_data$collection),
        multiple = FALSE,
        selected = "Labour Force"
      )
    })

    default_series <- reactive({
      selected_collection <- input$collection_select
      dplyr::case_when(selected_collection == "Consumer Price Index" ~
                         "Percentage Change from Corresponding Quarter of Previous Year ;  All groups CPI ;  Australia ; ;  Original",
                       selected_collection == "Wage Price Index" ~
                         "Percentage Change From Corresponding Quarter of Previous Year ;  Australia ;  Total hourly rates of pay excluding bonuses ;  Private and Public ;  All industries ; ;  Seasonally Adjusted",
                       selected_collection == "Labour Force" ~
                         "Unemployment rate ;  Persons ;  Australia ; ;  Seasonally Adjusted",
                       TRUE ~ "")
    })

    output$series_select <- renderUI({
      selectizeInput("series_select",
        label = "Select series",
        choices = series_within_collection(),
        multiple = TRUE,
        selected = as.character(default_series()),
        width = "100%",
        options = list(maxOptions = 10000)
      )
    })

    series_within_collection <- reactive({
      shiny::req(input$collection_select)
      available_data %>%
        dplyr::filter(.data$collection == input$collection_select) %>%
        dplyr::pull(.data$series)
    })

    selected_ids <- reactive({
      shiny::req(input$series_select)
      selected_series <- input$series_select
      as.character(available_data$series_id[available_data$series %in% selected_series])
    })

    chart_data <- reactive({
      shiny::req(selected_ids())
      dplyr::filter(data, .data$series_id %in% selected_ids() )
    })

    output$date_select <- renderUI({
      req(chart_data())
      min_data_date <- min(chart_data()$date)
      max_data_date <- max(chart_data()$date)
      sliderInput("date_select",
        label = "Select date range",
        min = min_data_date,
        max = max_data_date,
        value = c(min_data_date, max_data_date),
        timeFormat = "%b %Y",
        ticks = FALSE
      )
    })


    output$plot <- ggiraph::renderGirafe({
      req(chart_data())
      req(input$date_select)
      static_plot <- make_graph(
        chart_data = chart_data(),
        start_date = input$date_select[1],
        end_date = input$date_select[2]
      )

      ggiraph::girafe(ggobj = static_plot,
                      width_svg = 9,
                      height_svg = 5,
                      fonts = list(sans = "Roboto",
                                   serif = "Roboto"))
    })

    output$download_data <- downloadHandler(
      filename = function() {
        "wilma_data.csv"
      },
      content = function(file) {
        utils::write.csv(chart_data(), file = file)
      }
    )

    output$code <- renderText({
      ids <- paste0('"', selected_ids(), '"')
      ids <- paste0(ids, collapse = ", ")

      paste0(
        "readabs::read_abs(series_id = c(",
        ids,
        "))"
      )
    })
  }

  shinyApp(ui, server)
}
