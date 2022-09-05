ui_devop <- function(id='devop') {
  box::use(shiny)
  ns <- shiny$NS(id)
  shiny$inputPanel(
    shiny$actionButton(
      "deleteDevelopmentFolder", 
      "Delete Develoment Folder"
    ),
    shiny$actionButton(
      "createDevelopmentFolder", 
      "Create Develoment Folder"
    )
  )
}



#' @export
ui <- function() {
  box::use(shiny)
  box::use(../metadata/ui_metadata)
  shiny$fluidPage(
    shiny$titlePanel("CSM Management System"),
    shiny$sidebarLayout(
      shiny$sidebarPanel(
        width = 3,
        shiny$fluidRow(
          ui_metadata$ui(),
          {
            if (getOption("development")) {
              NULL
            } else {
              ui_devop()
            }
          }
        )
      ),
      shiny$mainPanel(
        shiny$tabsetPanel(
          shiny$tabPanel(
            "Raw Meta Data",
            shiny$tableOutput("outline")
          ),
          shiny$tabPanel(
            "Preview Data",
            shiny$uiOutput("previewData")
          )
        )
      )
    )
  )
}
