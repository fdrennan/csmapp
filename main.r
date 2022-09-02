
ui <- function() {
   box::use(shiny)
   box::use(./box/ui/meta)
   shiny$fluidPage(
      shiny$fluidRow(
         meta$ui_metadata()
      )
   )
}

server <- function(input, output, session) {
   box::use(./box/ui/meta, shiny)
   meta$server_metadata()
}

box::use(shiny)
shiny$shinyApp(ui, server)
