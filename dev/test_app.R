ui <- fluidPage(

  dateRangeInput("date_range", "Date Range", start = "2020-01-01", end = "2020-12-31"),

  shinyWidgets::radioGroupButtons(
    inputId = "date_range_btns",
    label = NULL,
    choices = list(
      "1Y"  = "12",
      "3Y"  = "36",
      "5Y"  = "60",
      "10Y" = "120",
      "All" = "1000"
    ),
    selected = "All",
    status = "primary",
    justified = TRUE,
    size = "sm"
  )

)

server <- function(input, output, session) {

  observe({

    updateDateRangeInput(
      session = session,
      inputId = "date_range",
      start = Sys.Date() - as.integer(input$date_range_btns),
      end = Sys.Date()
    )

  })
}

shinyApp(ui, server)
