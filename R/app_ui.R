#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    dashboardPage(
      header = header(),
      sidebar = sidebar(),
      controlbar = dashboardControlbar(),
      footer = dashboardFooter("This is the footer"),
      dark = FALSE,
      body = body()
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path("www", app_sys("app/www"))

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "ProCat"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

header <- function(){

  left_ui <- tagList(

    selectizeInput(
      inputId = "fund",
      label = NULL,
      choices = c(
        "FOGGEQUI",
        "FOGPRIV",
        "OGEUROP",
        "OLLCINTDE",
        "FOGCHEQ",
        "FOGASIA",
        "FOGASPAC",
        "FOGEMEQU"
      ),
      selected = character(0),
      multiple = FALSE,
      options = list(
        placeholder = "Select Fund"
      ),
      width = "140px"
    )

  )

  dashboardHeader(
    title = bs4DashBrand(
      title = "ProCat",
      color = "primary",
      href = NULL,
      image = "www/images/cat_logo_square_draft.png",
      opacity = 1
    ),
    leftUI = left_ui
  )

}

sidebar <-  function() {
  bs4DashSidebar(
    br(),
    sidebarMenu(
      menuItem("Welcome", tabName = "welcome"),
      menuItem("Brinson", tabName = "brinson", icon = icon("table"))
    )
  )
}

body <- function() {
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "welcome",
        htmltools::h1("Welcome to ProCat"),
        htmltools::p("This is a simple Shiny app to demonstrate the use of the golem package.")
      ),
      tabItem(
        tabName = "brinson",
        htmltools::h1("Brinson Analysis")
      )
    )
  )
}
