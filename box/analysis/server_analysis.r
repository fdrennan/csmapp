#' @export
server <- function(id, dataToAnalyze, parentSession) {
  box::use(shiny, cli, .. / lm / server_lm)
  box::use(shiny, cli, .. / lm / ui_lm, purrr)



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
          selector = paste0("#", ns("addButton")),
          where = "beforeBegin",
          ui = ui_lm$ui(ui_id, inputData())
        )
        server_lm$server(ui_id, parentSession, inputData)
      })
    },
    session = parentSession
  )
}


#' @export
# server <- function(id = "analysis") {
#   box::use(shiny, bs4Dash, cli)
#

#
#

# }
