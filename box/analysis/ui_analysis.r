#' @export
ui <- function(id = "analysis") {
  box::use(shiny)
  ns <- shiny$NS(id)
  # shiny$uiOutput(ns("previewData"))
  shiny$actionButton(
    ns("addButton"), "",
    icon = shiny$icon("plus")
  )
}
