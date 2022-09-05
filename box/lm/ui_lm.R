#' @export
ui <- function(id, data) {
  box::use(shiny)
  ns <- shiny$NS(id)
  shiny$fluidRow(
    shiny$column(
      4, 
      shiny$uiOutput(ns("lmModel"))
    ),
    shiny$column(
      8,
      shiny$uiOutput(ns('flaggingCode'))
    )
  )
}
