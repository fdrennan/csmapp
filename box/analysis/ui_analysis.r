#' @export
ui <- function(id, data) {
  # browser()
  box::use(shiny)
  box::use(shiny, cli, .. / lm / ui_lm)
  ns <- shiny$NS(id)
  shiny$fluidRow(
      shiny$column(
        12,
        shiny$uiOutput(ns("server_ui"))
      ),
      shiny$div(id=ns('paramSetup')),
      shiny$column(
        12,
        shiny$div(
          class='d-flex justify-content-end my-3',
          shiny$actionButton(
            class = "m-2",
            ns("addButton"),
            '',
            # shiny$div(class='text-right', "Add Flagging Criteria"),
            style = 'height: 3rem;',
            icon = shiny$icon("plus")
          )
        )
      )
  )
}
