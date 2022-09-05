#' @export
ui <- function(id) {
  box::use(shiny)
  box::use(shiny, cli, .. / lm / ui_lm)
  ns <- shiny$NS(id)
  bs4Dash::box(
    id = ns("boxId"),
    ui_lm$ui(ns(id), data),
    shiny$actionButton(
      ns("addButton"),
      "",
      icon = shiny$icon("plus")
    )
  )
}
