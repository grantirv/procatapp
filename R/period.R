# __________________________________________________________________________________________________
#' Period Selector
#'
#' Create a date range input with shortcut buttons for MTD, QTD, YTD, 1M, 3M, 6M, 1Y, 3Y, 5Y, 10Y,
#' and ALL. The dates can be set by selecting from the calendar, typing in the date, or clicking
#' shortcut buttons.
#'
#' @param id The id of the module
#' @param label The label of the date range input
#' @param width The width of the date range input and shortcut button group
#'
#' @returns The `input` function creates a date range input with shortcut buttons
#' @export
# __________________________________________________________________________________________________
period_input <- function(
  id,
  label = NULL,
  width = "480px"
){

  ns <- shiny::NS(id)

  date_range_input <- shiny::dateRangeInput(
    inputId = ns("date_range"),
    label = label,
    start = "1900-01-01",
    end = "2100-01-01",
    min = "1900-01-01",
    max = "2100-01-01",
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

  container <- div(
    id = ns("container"),
    class = "period_input_container",
    tags$head(tags$style(HTML(css))),
    date_range_input,
    btns
  )

  return(container)
}

# __________________________________________________________________________________________________
#' @rdname period_input
#'
#' @param id The id of the module
#' @inheritParams shiny::updateDateRangeInput
#'
#' @returns The `server` function returns a reactive date range
#' @export
# __________________________________________________________________________________________________
period_server <- function(id,
  min = "1900-01-01",
  max = "2100-01-01",
  start = min,
  end = max
){

  # preconditions
    stopifnot(!is.reactive(min))
    stopifnot(!is.reactive(max))
    stopifnot(!is.reactive(start))
    stopifnot(!is.reactive(end))

    min <- as.Date(min)
    max <- as.Date(max)
    start <- as.Date(start)
    end <- as.Date(end)

    checkmate::assert_date(min, upper = max, len = 1L, any.missing = FALSE)
    checkmate::assert_date(max, lower = min, len = 1L, any.missing = FALSE)
    checkmate::assert_date(start, lower = min, upper = end, len = 1L, any.missing = FALSE)
    checkmate::assert_date(end, lower = start, upper = max, len = 1L, any.missing = FALSE)

  moduleServer(id, function(input, output, session) {

    # update on shorcut button clicks
      observe({

        if (input$btns == "ALL") end <- max

        start <- switch(input$btns,
          "MTD"  = unname(dg::eop(end, "m", -1) + 1),
          "QTD"  = unname(dg::eop(end, "q", -1) + 1),
          "YTD"  = unname(dg::eop(end, "y", -1) + 1),
          "1M"   = clock::add_months(end, -1, invalid = "previous"),
          "3M"   = clock::add_months(end, -3, invalid = "previous"),
          "6M"   = clock::add_months(end, -6,   invalid = "previous"),
          "1Y"   = clock::add_years(end, -1, invalid = "previous"),
          "3Y"   = clock::add_years(end, -3, invalid = "previous"),
          "5Y"   = clock::add_years(end, -5, invalid = "previous"),
          "10Y"  = clock::add_years(end, -10, invalid = "previous"),
          "ALL"  = start
        )
        start <- max(min, start)

        shiny::updateDateRangeInput(
          session = session,
          inputId = "date_range",
          start = start,
          end = end,
          min = min,
          max = max
        ) |> cl("period_server", "shortcut_btns")

      }) |> bindEvent(input$btns)

    dates <- reactive({
      if (any(input$date_range == c("1900-01-01", "2100-01-01"))) {
        NULL
      } else {
        input$date_range
      }
    }) |> cl("period_server", "dates")

    return(dates)

  })

}

# test app
  period_app <- function(min = "2000-01-01", max = "2024-12-15") {
    ui <- fluidPage(
      br(),
      period_input("period"),
      textOutput("dates")
    )

    server <- function(input, output, session) {
      date_range <- period_server(id = "period", min = min, max = max)
      output$dates <- renderText({
        dts <- format(req(date_range()))
        glue::glue("{dts[1]} to {dts[2]}")
      })
    }

    shinyApp(ui, server)
  }
