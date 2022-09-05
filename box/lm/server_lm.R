

#' @export
server <- function(id, parentSession, inputData) {
  
  
  remove_shiny_inputs <- function(id, .input) {
    invisible(
      lapply(grep(id, names(.input), value = TRUE), function(i) {
        .subset2(.input, "impl")$.values$remove(i)
      })
    )
  }
  
  
  box::use(shiny, cli, bs4Dash, glue)
  shiny$moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      cli$cli_alert_info(ns("lmModel"))
      
      shiny$observeEvent(input$deleteButton, {
        shiny$removeUI(selector =  paste0('#',paste0(id, '-lmModel')))
        shiny$removeUI(selector =  paste0('#',ns('deleteButton')))
        remove_shiny_inputs(id, input)
      })
      
      
      output[["lmModel"]] <- shiny$renderUI({
        shiny$req(inputData)
        data <- inputData()
        
        PARAMCD <- data[[1]]$PARAMCD
        analysis <- data[[1]]$analysis
        
        shiny$fluidRow(
          id = environment(ns)[["namespace"]],
          bs4Dash$box(title = id,
            width = 12,
            shiny$selectizeInput(ns("statsGroupPARAMCD"),
              shiny$h4(glue$glue("PARAMCD")),
              choices = data[[1]]$PARAMCD,
              selected = data[[1]]$PARAMCD, multiple = TRUE
            ),
            shiny$div(
              shiny$fluidRow(
                shiny$numericInput(
                  ns("nStatistics"), "n", min = -Inf, max = Inf, value = 2
                ),
                shiny$numericInput(
                  ns("rStatistics"), "r", min = -Inf, max = Inf, value = 2
                ),
                shiny$numericInput(
                  ns("diff_pctStatistics"), "diff_pct", 
                  min = -Inf, max = Inf, value = 10
                )
              ),
              shiny$fluidRow(
                shiny$actionButton(
                  ns("deleteButton"),
                  "",
                  icon = shiny$icon("minus")
                ),
              )
            )
          )
        )
      })
    },
    session = parentSession
  )
}
