#' @export
ui <- function(id, data) {
  box::use(shiny)
  ns <- shiny$NS(id)
  shiny$fluidRow(
    shiny$column(
      6, 
      shiny$uiOutput(ns("lmModel"))
    ),
    shiny$column(
      6,
      shiny$uiOutput(ns('flaggingCode'))
    )
  )
}
