

#' @export
server <- function(id, parentSession, inputData) {
  # browser()
  box::use(shiny, cli)
  shiny$moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      cli$cli_alert_info(ns("lmModel"))
      output[["lmModel"]] <- shiny$renderUI({
        # browser()
        shiny$req(inputData)
        data <- inputData()
        PARAMCD <- data[[1]]$PARAMCD
        shiny$div(
          id = environment(ns)[["namespace"]],
          shiny$fluidRow(
            shiny$h1(ns(id)),
            shiny$selectizeInput(ns('selectizeParamcd'), 'Paramcd', choices = PARAMCD, selected = PARAMCD, multiple=TRUE)
          )
        )
      })
    },
    session = parentSession
  )
}
