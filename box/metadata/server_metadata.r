
#' @export
server <- function(id = "metadata") {
  box::use(shiny, dplyr, stats, bs4Dash)
  shiny$moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      datafiles <- shiny$reactive({
        
        box::use(.. / caching / cache)
        datafiles <- cache$check()
        datafiles
      })

      output$study <- shiny$renderUI({
        
        shiny$req(datafiles())
        datafiles <- datafiles()
        study <- datafiles$study

        shiny$selectizeInput(ns("study"), shiny$h5("Study"),
          choices = study,
          selected = study[1], multiple = FALSE
        )
      })

      output$year <- shiny$renderUI({
        
        shiny$req(input$study)
        datafiles <- datafiles()
        year <- datafiles |>
          dplyr$filter(study %in% input$study) |>
          dplyr$pull(year)

        shiny$selectizeInput(ns("year"), "Year", choices = year, selected = year[1], multiple = FALSE)
      })

      output$month <- shiny$renderUI({
        
        datafiles <- datafiles()
        shiny$req(input$year)
        monthName <- datafiles |>
          dplyr$filter(study %in% input$study, year %in% input$year) |>
          dplyr$pull(monthName)

        max_month <- datafiles |>
          dplyr$filter(date == max(date)) |>
          dplyr$pull(monthName)
        shiny$selectizeInput(ns("monthName"), "Month", choices = monthName, selected = max_month, multiple = TRUE)
      })

      output$analysis <- shiny$renderUI({
        
        shiny$req(input$monthName)
        datafiles <- datafiles()
        analysis <- datafiles |>
          dplyr$filter(
            study %in% input$study, year %in% input$year,
            monthName %in% input$monthName
          ) |>
          dplyr$pull(analysis)

        shiny$div(
          shiny$selectizeInput(ns("analysis"), "Analysis",
            choices = analysis,
            selected = analysis, multiple = TRUE
          ),
          bs4Dash$actionButton(ns("go"), "Go")
        )
      })

      filteredData <- shiny$eventReactive(
        input$go,
        {
          
          datafiles <- datafiles()
          files <- datafiles |>
            dplyr$filter(
              study %in% input$study,
              year %in% input$year,
              monthName %in% input$monthName,
              analysis %in% input$analysis
            )
          files
        }
      )

      out <- shiny$reactive({
        
        tryCatch({
          
          filteredData()
        }, error = function(err) {
          
          shiny$showNotification('Error')
          FALSE
        })
      })
      out
    }
  )
}
