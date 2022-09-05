

#' @export
server <- function(id, parentSession, data) {
  box::use(shiny, cli)
  shiny$moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      cli$cli_alert_info(ns("lmModel"))
      output[["lmModel"]] <- shiny$renderUI({
        shiny$div(
          id = environment(ns)[["namespace"]],
          shiny$fluidRow({
            # browser()
            shiny$h1('asdfasd')
          })
        )
      })
    },
    session = parentSession
  )
}
