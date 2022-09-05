#' @export
server <- function(id, dataToAnalyze, parentSession) {
  box::use(shiny, cli, .. / lm / server_lm)
  box::use(shiny, cli, .. / lm / ui_lm, purrr)
  
  
  remove_shiny_inputs <- function(id, .input) {
    invisible(
      lapply(grep(id, names(.input), value = TRUE), function(i) {
        .subset2(.input, "impl")$.values$remove(i)
      })
    )
  }
  
  
  
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

      shiny$observeEvent(input$addButton, {
        i <- sprintf("%04d", input$addButton)
        id <- sprintf("%s", i)
        ui_id <- ns(id)
        shiny$insertUI(
          selector = paste0("#", ns("addButton")),
          where = "beforeBegin",
          ui = ui_lm$ui(ui_id, data)
        )
        server_lm$server(ui_id, parentSession, inputData)
        
        shiny$observeEvent(input$deleteButton, {
          shiny$showNotification('TODO: Set up delete button')
        })
        shiny$observeEvent(input$deleteButton, {
          shiny$removeUI(selector =  paste0('#',ui_id))
          remove_shiny_inputs(id, input)
        })
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
