#' @export
server <- function(id, dataToAnalyze, parentSession, sessionId) {
  #
  box::use(shiny, cli, .. / lm / server_lm)
  box::use(shiny, cli, .. / lm / ui_lm, purrr, stringr, dplyr, tidyr)

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
          where = "beforeBegin",
          ui = ui_lm$ui(ui_id, inputData())
        )
        server_lm$server(ui_id, parentSession, inputData)
      })

      shiny$observeEvent(input$finishSetup, {
        # browser()
        out <- shiny$reactiveValuesToList(input)
        storr <- storr::storr_rds("storr")
        storrId <- paste0(id, "-", sessionId)
        storr$set(storrId, out, use_cache = FALSE)
        shiny$showNotification(paste("Data stored at", storrId))
      })
    },
    session = parentSession
  )
}
