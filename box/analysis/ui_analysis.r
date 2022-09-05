#' @export
ui <- function(id = "dynamic_module") {
  box::use(shiny)
  ns <- shiny$NS(id)
  bs4Dash::box(
    shiny$actionButton(ns("addButton"), "", icon = shiny$icon("plus"))
  )
}



#' #' @export
#' ui <- function(id = "analysis") {
#'
#' }
