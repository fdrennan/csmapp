#' @export
server <- function(id, dataToAnalyze, parentSession) {
  #
  box::use(shiny, cli, .. / lm / server_lm)
  box::use(shiny, cli, .. / lm / ui_lm, purrr, stringr, dplyr)

  shiny$moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      inputData <- shiny$reactive({
        shiny$req(dataToAnalyze)
        data <- purrr$keep(dataToAnalyze, function(x) {
          x$analysis == id
        })
      })

      updateId <- shiny$eventReactive(input$addButton, {
        i <- sprintf("%04d", input$addButton)
        id <- sprintf("%s", i)
        ui_id <- ns(id)
      })

      shiny$observeEvent(input$addButton, {
        shiny$req(updateId())
        shiny$req(inputData())
        ui_id <- updateId()
        shiny$insertUI(
          selector = paste0("#", ns("paramSetup")),
          where = "beforeEnd",
          ui = ui_lm$ui(ui_id, inputData())
        )
        server_lm$server(ui_id, parentSession, inputData)
      })
      
      shiny$observeEvent(input$finishSetup, {
        browser()
        out <- shiny$reactiveValuesToList(input)
        value_names <- names(out)
        lapply(value_names, function(x) shiny$showNotification(x))
        flags <- out[grepl('^[0-9]{4}-flagValue', value_names)]
        flagCode <- out[grepl('^[0-9]{4}-flagCode', value_names)]
        PARAMCD <- out[grepl('^[0-9]{4}-statsGroupPARAMCD', value_names)]
       
        
        flags <- dplyr$tibble(
          name = names(flags),
          value = unlist(flags)
        )
        
        flagCode <- dplyr$tibble(
          name = names(flagCode),
          value = unlist(flagCode)
        )
        
        PARAMCD <- dplyr$tibble(
          name = names(PARAMCD),
          value = unlist(PARAMCD)
        )
        
        out <- dplyr$bind_rows(
          flags,
          flagCode, PARAMCD
        )
        
        print(out)
      
      })
      
      
    },
    session = parentSession
  )
}
