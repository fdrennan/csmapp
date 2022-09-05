#' @export
ui <- function(id) {
  box::use(shiny)
  ns <- shiny$NS(id)
  shiny$uiOutput(ns("lmModel"))
}