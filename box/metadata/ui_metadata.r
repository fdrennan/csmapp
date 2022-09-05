#' @export
ui <- function(id = "metadata") {
  box::use(shiny)
  ns <- shiny$NS(id)
  shiny$div(
    shiny$actionButton(ns("reset"), "Reset"),
    shiny$uiOutput(ns("study")),
    shiny$uiOutput(ns("year")),
    shiny$uiOutput(ns("month")),
    shiny$uiOutput(ns("analysis")),
    shiny$actionButton(ns("go"), "Go")
  )
}
