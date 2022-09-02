#' @export
ui_metadata <- function(id='metadata') {
  box::use(shiny)
  ns <- shiny$NS(id)
  shiny$div(
    shiny$checkboxInput(ns('selectAll'), 'Select All', TRUE),
    shiny$actionButton(ns('reset'), 'Reset'),
    shiny$uiOutput(ns('study')),
    shiny$uiOutput(ns('year')),
    shiny$uiOutput(ns('month')),
    shiny$tableOutput(ns('outline'))
  )
}

#' @export
server_metadata <- function(id='metadata') {
  box::use(shiny, dplyr, stats)
  shiny$moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      datafiles <- shiny$reactive({
        box::use(../cache)
        input$reset
        datafiles <- cache$check()
      })
      
      output$study <- shiny$renderUI({
        shiny$req(datafiles())
        datafiles <- datafiles()
        study <- datafiles()$study
        if (input$selectAll) selected <-  study else selected = NULL
        shiny$selectizeInput(ns('study'), 'Study', choices=study, selected=selected, multiple=TRUE)
      })
      
      output$year <- shiny$renderUI({
        shiny$req(input$study)
        datafiles <- datafiles()
        year <- datafiles |> 
          dplyr$filter(study %in% input$study) |> 
          dplyr$pull(year)
        if (input$selectAll) selected <-  year else selected = NULL
        shiny$selectizeInput(ns('year'), 'Year', choices=year, selected=selected, multiple=TRUE)
      })
      
      output$month <- shiny$renderUI({
        datafiles <- datafiles()
        month <- datafiles |> 
          dplyr$filter(study %in% input$study, year %in% input$year) |> 
          dplyr$pull(month)
        month <- stats$setNames(month, month.name[month])
        if (input$selectAll) selected <-  month else selected = NULL
        shiny$selectizeInput(ns('month'), 'Month', choices=month, selected=selected, multiple=TRUE)
      })
      
      output$outline <- shiny$renderTable({
        datafiles <- datafiles()
        files <- datafiles |> 
          dplyr$filter(study %in% input$study, year %in% input$year, month %in% input$month) 
        
        files
      })
      
      datafiles
    }
  )
  
}