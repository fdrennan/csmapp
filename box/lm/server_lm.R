

#' @export
server <- function(id, parentSession, inputData) {
  # browser()
  box::use(shiny, cli, bs4Dash, glue)
  shiny$moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      cli$cli_alert_info(ns("lmModel"))
      output[["lmModel"]] <- shiny$renderUI({
        shiny$req(inputData)
        data <- inputData()
        PARAMCD <- data[[1]]$PARAMCD
        analysis <- data[[1]]$analysis
        shiny$fluidRow(
          id = environment(ns)[["namespace"]],
          bs4Dash$box(
            width = 12,
            shiny$selectizeInput(ns("statsGroupPARAMCD"),
              glue$glue("Stats Group"),
              choices = data[[1]]$PARAMCD,
              selected = data[[1]]$PARAMCD, multiple = TRUE
            ),
            shiny$fluidRow(
              shiny$column(
                6,
                shiny$numericInput(ns("nStatistics"),
                  "n",
                  min = -Inf, max = Inf, value = 2
                )
              ),
              shiny$column(
                6,
                shiny$numericInput(ns("rStatistics"),
                  "r",
                  min = -Inf, max = Inf, value = 2
                )
              ),
              shiny$column(
                6,
                shiny$numericInput(ns("diff_pctStatistics"),
                  "diff_pct",
                  min = -Inf, max = Inf, value = 10
                )
              )
            )
          )
        )
      })
    },
    session = parentSession
  )
}
