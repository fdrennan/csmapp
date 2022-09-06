#' @export
ui <- function(id, data) {
  #
  box::use(.. / lm / ui_lm)
  box::use(shiny, cli, bs4Dash)
  ns <- shiny$NS(id)
  shiny$fluidRow(
    shiny$div(id = ns("paramSetup")),
    shiny$column(
      12,
      shiny$div(
        class = "text-right",
        bs4Dash$actionButton(
          ns("addButton"),
          "",
          style = "height: 3rem;",
          icon = shiny$icon("plus")
        )
      )
    )
  )
}
