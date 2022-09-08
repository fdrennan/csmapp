#' @export
ui <- function(id, data) {
  box::use(shiny)
  ns <- shiny$NS(id)
  shiny$uiOutput(ns("lmModel"), class = "col-6")
}
