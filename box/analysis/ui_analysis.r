#' @export
ui <- function(id) {
  box::use(shiny)
  ns <- shiny$NS(id)
  bs4Dash::box(id = ns('boxId'),
    shiny$actionButton(
      ns("addButton"), 
      "", 
      icon = shiny$icon("plus")
    )
  )
}

