

#' @export
server <- function(id, parentSession) {
  box::use(shiny, cli)
  shiny$moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      cli$cli_alert_info(ns("lmModel"))
      output[["lmModel"]] <- shiny$renderUI({
        shiny$div(
          id = environment(ns)[["namespace"]],
          shiny$div(
            environment(ns)[["namespace"]]
          )
        )
      })
    },
    session = parentSession
  )
}
