# __________________________________________________________________________________________________
#' Brinson Attribution UI
#'
#' Create the UI for Brinson attribution analysis. The UI includes a row field, column field, date
#' range input, and a table output.
#'
#' @param id The id of the module
#'
#' @returns The `brinson_ui` function returns a UI for Brinson attribution analysis.
#' @export
# __________________________________________________________________________________________________
brinson_ui <- function(
  id
){

  ns <- shiny::NS(id)

  # select row field
    row <- div(style = "grid-area: row;",
      selectizeInput(
        inputId = ns("rows"),
        label = "Rows",
        choices = list(
          "RGN"   = "rgn",
          "CTRY"  = "ctry",
          "GICS1" = "gics1",
          "GICS2" = "gics2",
          "GICS3" = "gics3",
          "GICS4" = "gics4",
          "STOCK" = "infocode"
        ),
        selected = "gics1",
        multiple = FALSE,
        options = list(
          placeholder = "Select row field"
        ),
        width = "100%"
      )
    )

  # select column field
    column <- div(style = "grid-area: column;",
      selectizeInput(
        inputId = ns("columns"),
        label = "Columns",
        choices = list(
          "Effects" = "effect",
          "Year"    = "year",
          "Quarter" = "quarter",
          "Month"   = "month"
        ),
        selected = "effect",
        multiple = FALSE,
        options = list(
          placeholder = "Select column field"
        ),
        width = "100%"
      )
    )

  # select date range for analysis
    period <- div(style = "grid-area: period;",
      period_input(id = ns("period"), label = "Period", width = "500px")
    )

  # display brinson table
    table <- div(style = "grid-area: table; justify-self: start;",
      gt::gt_output(ns("table"))
    )

  # layout
    layout <- div(style = "
      display: grid;
      grid-template-columns: 200px 200px 1fr;
      grid-template-rows: auto 1fr;
      column-gap: 20px;
      row-gap: 20px;
      grid-template-areas:
        'row   column period'
        'table table  table';
    ",
      row,
      column,
      period,
      table
    )

  return(layout)
}

# __________________________________________________________________________________________________
#' @rdname brinson_ui
#'
#' @description
#' The brinson server logic creates an attribution table based on the row, column and date
#' selections and renders the table.
#'
#' @param id The id of the module
#' @param atb_data A reactive [gpa::AttributionData] object
#'
#' @returns The `server` function has no return value.
# __________________________________________________________________________________________________
brinson_server <- function(
  id,
  atb_data
){
  # preconditions
    stopifnot(!is.reactive(atb_data))
    id <- checkmate::assert_string(id)

  moduleServer(id, function(input, output, session){

    # select date range
      date_range <- period_server(
        id = "period",
        min = atb_data$data[, min(date)],
        max = atb_data$data[, max(date)]
      )

    # subset atb_data
      atb_data_slice <- reactive({
        dts <- req(date_range())
        atb_data$clone()$date_subset(start_date = dts[1], end_date = dts[2])
      }) |> cl("brinson_server", "atb_data_slice")

    # build formula
      formula <- reactive({
        as.formula(paste(input$rows, "~", input$columns))
      }) |> cl("brinson_server", "formula")

    # create attribution table
      atb_table <- reactive({
        gpa::AttributionTable$new(
          atb_data = atb_data_slice(),
          formula = formula(),
          filter = NULL
        )
      }) |> cl("brinson_server", "atb_table")

    # create table output
      output$table <- gt::render_gt({
        atb_table()$tbl
      }) |> cl("brinson_server", "output_table")

  })

}

# test app
  brinson_app <- function(
    id = "brinson",
    atb_data = gpa::europe_3y
  ){

    ui <- fluidPage(
      br(),
      brinson_ui(id)
    )

    server <- function(input, output, session){
      brinson_server(id = id, atb_data = atb_data)
    }

    shinyApp(ui, server)
  }
