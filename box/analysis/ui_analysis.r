#' @export
ui <- function(id, data) {
  box::use(shiny)
  box::use(shiny, cli, .. / lm / ui_lm)
  ns <- shiny$NS(id)
  shiny$div(
    shiny$actionButton(
      class = "m-2",
      ns("addButton"),
      "",
      icon = shiny$icon("plus")
    ),
    shiny$uiOutput(ns("server_ui"))
  )
}
