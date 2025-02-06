# __________________________________________________________________________________________________
#' procatapp package
#'
#' ProCat enables interactive process attribution for 4F funds. If you can't measure it, you can't
#' manage it!
#'
#' @name procatapp-package
#' @author Grant Irvine-Smith \email{grant.irvine-smith@@ninetyone.com}
#' @import data.table
#' @import ggplot2
#' @import bs4Dash
#' @importFrom magrittr `%>%`
#' @importFrom shiny br
#' @importFrom shiny bindCache
#' @importFrom shiny bindEvent
#' @importFrom shiny code
#' @importFrom shiny conditionalPanel
#' @importFrom shiny debounce
#' @importFrom shiny div
#' @importFrom shiny downloadButton
#' @importFrom shiny downloadHandler
#' @importFrom shiny fluidRow
#' @importFrom shiny fluidPage
#' @importFrom htmltools h5
#' @importFrom shiny icon
#' @importFrom shiny img
#' @importFrom shiny isolate
#' @importFrom shiny includeMarkdown
#' @importFrom shiny is.reactive
#' @importFrom shiny isTruthy
#' @importFrom shiny moduleServer
#' @importFrom shiny observe
#' @importFrom shiny observeEvent
#' @importFrom shiny plotOutput
#' @importFrom shiny reactive
#' @importFrom shiny reactiveVal
#' @importFrom shiny renderText
#' @importFrom shiny renderPlot
#' @importFrom shiny req
#' @importFrom shiny selectizeInput
#' @importFrom shiny selectInput
#' @importFrom shiny shinyOptions
#' @importFrom shiny sliderInput
#' @importFrom shiny tabPanel
#' @importFrom shiny tableOutput
#' @importFrom shiny textOutput
#' @importFrom shiny updateSelectizeInput
# __________________________________________________________________________________________________
"_PACKAGE"

globalVariables(c(
  "..cols",
  ".",
  "J"
))
