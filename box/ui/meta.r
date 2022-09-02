#' @export
ui_metadata <- function(id='metadata') {
  box::use(shiny)
  ns <- shiny$NS(id)
  shiny$div(
    shiny$checkboxInput(ns('selectAll'), 'Select All', FALSE),
    shiny$checkboxInput(ns('linkedOnly'), 'Linked Only', TRUE),
    shiny$actionButton(ns('reset'), 'Reset'),
    shiny$uiOutput(ns('study')),
    shiny$uiOutput(ns('year')),
    shiny$uiOutput(ns('month')),
    shiny$uiOutput(ns('analysis')),
    shiny$actionButton(ns('go'), 'Go')
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
        input$reset
        box::use(../cache)
        datafiles <- cache$check()
        if (input$linkedOnly) {
          datafiles <- datafiles |> 
            dplyr$filter(!is.na(analysis))
        }
        datafiles
      })
      
      output$study <- shiny$renderUI({
        shiny$req(datafiles())
        datafiles <- datafiles()
        study <- datafiles()$study
        
        shiny$selectizeInput(ns('study'), 'Study', choices=study, selected=study, multiple=TRUE)
      })
      
      output$year <- shiny$renderUI({
        shiny$req(input$study)
        datafiles <- datafiles()
        year <- datafiles |> 
          dplyr$filter(study %in% input$study) |> 
          dplyr$pull(year)
        if (input$selectAll) selected <-  year else selected = max(year)
        shiny$selectizeInput(ns('year'), 'Year', choices=year, selected=selected, multiple=TRUE)
      })
      
      output$month <- shiny$renderUI({
        datafiles <- datafiles()
        shiny$req(input$year)
        monthName <- datafiles |> 
          dplyr$filter(study %in% input$study, year %in% input$year) |> 
          dplyr$pull(monthName)
        
        if (input$selectAll) {
          selected <-  monthName
        } else selected = {
          max_month <- datafiles |> 
            dplyr$filter(date==max(date)) |> 
            dplyr$pull(monthName) 
        }
        shiny$selectizeInput(ns('monthName'), 'Month', choices=monthName, selected=selected, multiple=TRUE)
      })
      
      output$analysis <- shiny$renderUI({
        shiny$req(input$monthName)
        datafiles <- datafiles()
        analysis <- datafiles |> 
          dplyr$filter(study %in% input$study, year %in% input$year,
                       monthName %in% input$monthName) |> 
          dplyr$pull(analysis)
        
        shiny$selectizeInput(ns('analysis'), 'Analysis', choices=analysis, selected=analysis, multiple=TRUE)
      })
      
      filteredData <- shiny$eventReactive(
        input$go, {
          datafiles <- datafiles()
          
          files <- datafiles |> 
            dplyr$filter(study %in% input$study, 
                         year %in% input$year, 
                         monthName %in% input$monthName,
                         analysis %in% input$analysis) 
          files
        }
      )
      

      filteredData
    }
  )
  
}