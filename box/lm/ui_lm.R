#' @export
ui <- function(id, data) {
  box::use(shiny)
  ns <- shiny$NS(id)
  shiny$div(
    shiny$uiOutput(ns("lmModel")),
    shiny$actionButton(
      ns("deleteButton"),
      "",
      icon = shiny$icon("minus")
    )
  )
}
