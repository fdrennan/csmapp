#' @export
ui <- function(id) {
  box::use(shiny)
  box::use(shiny, cli, .. / lm / ui_lm)
  ns <- shiny$NS(id)
  bs4Dash::box(
    id = ns("boxId"), title = id,
    shiny$uiOutput(ns('server_ui')),
    shiny$actionButton(
      ns("addButton"),
      "",
      icon = shiny$icon("plus")
    )
  )
}
