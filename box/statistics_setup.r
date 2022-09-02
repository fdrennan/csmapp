#' @export
ui_statistics_setup <- function(id = "statistics_setup") {
  box::use(shiny)
  ns <- shiny$NS(id)
  shiny$div(
    shiny$uiOutput(ns("ui"))
  )
}

#' @export
server_statistics_setup <- function(id = "statistics_setup", modInput, previewData) {
  box::use(shiny, purrr, stringr)
  shiny$moduleServer(
    id,
    function(input, output, session) {
      ns <- session
      output$ui <- shiny$renderUI({
        shiny$req(modInput())
        
        data <- modInput()
        pd <- previewData()
        
        
        data <- 
          purrr$map(
          pd, 
          function(x) {
            analysis <- x[[3]]
            data[
              stringr$str_detect(
                names(data),
                paste0('^', analysis)
              )
            ]
          }
        )
        browser()
        purrr$map(
          data,
          function(x) {
            purrr$map(
              1:(x[[2]]),
              function(y) {
                shiny$showNotification(y)
              }
            )
          }
        )
        
        
        shiny$div('done')
      })
    }
  )
}