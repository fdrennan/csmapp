#' @export
ui <- function(id, data) {
  box::use(shiny)
  ns <- shiny$NS(id)
  shiny$div(
    shiny$uiOutput(ns("lmModel"))
  )
}
