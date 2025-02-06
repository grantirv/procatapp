period_ui <- function(
  id,
  start_date = NULL,
  end_date = NULL,
  label = NULL,
  width = "480px"
){

  ns <- NS(id)

  date_range_input <- dateRangeInput(
    inputId = ns("date_range"),
    label = label,
    start = start_date,
    end = end_date,
    width = width
  )

  btns <- shinyWidgets::radioGroupButtons(
    inputId = ns("btns"),
    label = NULL,
    choices = list("MTD", "QTD", "YTD", "1M", "3M", "6M", "1Y", "3Y", "5Y", "10Y", "ALL"),
    selected = "ALL",
    status = "primary",
    justified = TRUE,
    size = "sm",
    width = width
  )

  css <- glue::glue("

    #{ns('date_range')} {{
      margin-bottom: 0px;
    }}

    #{ns('date_range')} > div > input:first-child{{
      border-top-left-radius: 5px;
      border-bottom-left-radius: 0px;
    }}

    #{ns('date_range')} > div > input:last-child{{
      border-top-right-radius: 5px;
      border-bottom-right-radius: 0px;
    }}

    #{ns('btns')} > div > div:first-child > button{{
      border-top-left-radius: 0px;
      border-bottom-left-radius: 5px;
    }}

    #{ns('btns')} > div > div:last-child > button{{
      border-top-right-radius: 0px;
      border-bottom-right-radius: 5px;
    }}

  ")

  out <- div(
    tags$head(tags$style(HTML(css))),
    date_range_input,
    btns
  )

  return(out)
}

period_server <- function(id, start_date, end_date){

  date_range <- moduleServer(id, function(input, output, session) {

  # initialize date range input
    updateDateRangeInput(
      session = session,
      inputId = "date_range",
      start = start_date,
      end = end_date
    )

  # update on shorcut button clicks
    observe({

      start <- input$date_range[1]
      end <- input$date_range[2]

      start <- switch(input$btns,
        "MTD"  = unname(dg::eop(end, "m", -1) + 1),
        "QTD"  = unname(dg::eop(end, "q", -1) + 1),
        "YTD"  = unname(dg::eop(end, "y", -1) + 1),
        "1M"   = clock::add_months(end, -1, invalid = "previous"),
        "3M"   = clock::add_months(end, -3, invalid = "previous"),
        "6M"   = clock::add_months(end, -6, invalid = "previous"),
        "1Y"   = clock::add_years(end, -1, invalid = "previous"),
        "3Y"   = clock::add_years(end, -3, invalid = "previous"),
        "5Y"   = clock::add_years(end, -5, invalid = "previous"),
        "10Y"  = clock::add_years(end, -10, invalid = "previous"),
        "ALL"  = start_date
      )

      if (input$btns == "ALL") {
        end <- end_date
      }

      updateDateRangeInput(
        session = session,
        inputId = "date_range",
        start = start,
        end = end
      )

    }) |> bindEvent(input$btns)

  date_range <- reactive({
    input$date_range
  })

  return(date_range)

  })

  return(date_range)
}

period_app <- function() {
  ui <- fluidPage(
    br(),
    period_ui("period"),
    verbatimTextOutput("dates")
  )

  server <- function(input, output, session) {
    date_range <- period_server("period", start_date = "2020-01-01", end_date = "2020-12-31")
    output$dates <- renderPrint({
      date_range()
    })
  }

  shinyApp(ui, server)
}

period_app()
