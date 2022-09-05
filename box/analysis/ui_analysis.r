#' @export
ui <- function(id='dynamic_module') {
  box::use(shiny)
  ns <- shiny$NS(id)
  shiny$actionButton(ns("addButton"), "", icon = shiny$icon("plus"))
}



#' #' @export
#' ui <- function(id = "analysis") {
#'   
#' }
