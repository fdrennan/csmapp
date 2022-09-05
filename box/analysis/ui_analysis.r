#' @export
ui <- function(id, data) {
  box::use(shiny)
  box::use(shiny, cli, .. / lm / ui_lm)
  ns <- shiny$NS(id)
  shiny$fluidRow(
    shiny$column(
      12,
      shiny$fluidRow(
        class='d-flex justify-content-end align-items-end',
        shiny$actionButton(class='m-2',
                           ns("addButton"),
                           "",
                           icon = shiny$icon("plus")
        )
      )
    ),
    shiny$column(
      12,
      shiny$uiOutput(ns("server_ui"))
    )
  )
}
