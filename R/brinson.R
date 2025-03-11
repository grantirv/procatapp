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

  # select analysis period
    analysis_period <- period_input(id = ns("period"), label = NULL, width = "500px")

  # select allocation field - this will be used to measure the allocation effect
    rows <- selectizeInput(
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
        placeholder = "Select allocation field"
      ),
      width = "120px"
    )

  # select group_by field - this splits the effects into different groups displayed as columns
    columns <- selectizeInput(
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
      width = "120px"
    )

  # set max rows to display - remainder are collapsed into 'Other'
    max_rows <- selectizeInput(
      inputId = ns("max_rows"),
      label = "Max Rows",
      choices = list(
        "10"    = 10,
        "20"    = 20,
        "50"    = 50,
        "100"   = 100,
        "500"   = 500,
        "1000"  = 1000,
        "ALL"   = Inf
      ),
      selected = "50",
      multiple = FALSE,
      options = list(
        placeholder = "Select max rows to display"
      ),
      width = "100px"
    )

  # display brinson table
    table <- gt::gt_output(ns("table")) |>
      tagAppendAttributes(class = "gt_brinson")

  # group controls together
    controls <- div(
      id = ns("controls"),
      class = "brinson_controls",
      style = "
        display: flex;
        flex-direction: row;
        justify-content: flex-start;
        align-items: flex-end;
        column-gap: 20px;
        flex-wrap: wrap;
      ",
      analysis_period,
      rows,
      columns,
      max_rows
    ) |> shiny::wellPanel(style = "paddding: 10px !important;")

  # fine tune styling
    css <- glue::glue("
      #{ns('container')} .shiny-input-container {{
        margin-bottom: 0px;
      }}
    ")

    css <- tags$head(tags$style(HTML(css)))

    container <- div(
      id = id,
      class = "brinson_container",
      css,
      controls,
      table
    )

  return(container)
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

    # max_rows
      max_rows <- reactive({
        as.numeric(input$max_rows)
      })

    # sort_by
      sort_by <- reactive({
        if (max_rows() == Inf) {
          "name"
        } else {
          "ctb"
        }
      })

    # create attribution table
      atb_table <- reactive({
        gpa::AttributionTable$new(
          atb_data = atb_data_slice(),
          formula = formula(),
          filter = NULL,
          id = gt::random_id(),
          height = "100%",
          max_rows = max_rows(),
          sort_by = sort_by(),
          freeze_panes = c("top", "bottom", "left", "right"),
          highlight_on_hover = TRUE
        )
      }) |> cl("brinson_server", "atb_table")

    # create table output
      output$table <- gt::render_gt({
        tbl <- atb_table()$tbl
        atb_table()$gt
      },
      height = "calc(100vh - 300px)",
      width = "calc(100vw - 100px)",
      align = "left"
      ) |> cl("brinson_server", "output_table")

    # get clicked cell
      clicked_cell <- reactive({
        input$clicked_cell # set in brinson.js
      }) |> cl("brinson_server", "clicked_cell")

    clicked_cell
  })

}

# test app
  brinson_app <- function(
    id = "brinson",
    atb_data = gpa::europe_3y
  ){
    ui <- fluidPage(
      golem_add_external_resources(),
      brinson_ui(id),
      shiny::verbatimTextOutput("clicked_cell", placeholder = TRUE),
      actionButton("debug", "Debug")
    )

    server <- function(input, output, session){
      clicked_cell <- brinson_server(id = id, atb_data = atb_data)
      output$clicked_cell <- renderPrint({
        clicked_cell()
      })
      observeEvent(input$debug, browser())
    }

    shinyApp(ui, server)
  }
