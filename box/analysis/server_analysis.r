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

      shiny$observeEvent(input$addButton, {

        i <- sprintf("%04d", input$addButton)
        id <- sprintf("%s", i)
        shiny$insertUI(
          selector = paste0("#", ns("addButton")),
          where = "beforeBegin",
          ui = ui_lm$ui(ns(id), data)
        )
        server_lm$server(ns(id), parentSession, inputData)
        shiny$observeEvent(input$deleteButton, {
          shiny$showNotification('TODO: Set up delete button')
        })
        # shiny$observeEvent(input[[ns("deleteButton")]], {
        #   shiny$removeUI(selector = sprintf("#%s", id))
        #   remove_shiny_inputs <- function(id, .input) {
        #     invisible(
        #       lapply(grep(id, names(.input), value = TRUE), function(i) {
        #         .subset2(.input, "impl")$.values$remove(i)
        #       })
        #     )
        #   }
        # 
        #   remove_shiny_inputs(id, input)
        # })
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
