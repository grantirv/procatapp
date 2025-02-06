#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @noRd
app_server <- function(input, output, session) {

  # fetch attribution data
    atb_data <- reactive({
      gpa::read_atb_data(
        path = file.path(golem::get_golem_options("app_data_path"), "atb_data", input$fund)
      )
    })

  # get analysis period
    date_range <- period_server(
      id = "period",
      start_date = "2010-01-01",
      end_date = "2024-12-31"
    )

  # create attribution table
    atb_table <- reactive({
      gpa::AttributionTable$new(
        atb_data = atb_data(),
        formula = gics1 ~ effect,
        filter = NULL
      )
    })

  # create brinson table output
    output$brinson_table <- gt::render_gt({
      atb_table()$tbl
    })

}
