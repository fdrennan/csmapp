#' @export
ui_statistics_setup <- function(id = "statistics_setup") {
  box::use(shiny)
  ns <- shiny$NS(id)
  shiny$div(
    shiny$uiOutput(ns("ui"))
  )
}

#' @export
server_statistics_setup <- function(id = "statistics_setup", modInput) {
  box::use(shiny)
  shiny$moduleServer(
    id,
    function(input, output, session) {
      ns <- session
      output$ui <- shiny$renderUI({
        shiny$req(modInput())
        shiny$div('helllo')
        
      })
    }
  )
}