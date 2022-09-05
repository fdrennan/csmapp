

#' @export
server <- function(id, parentSession, inputData) {
  remove_shiny_inputs <- function(id, .input) {
    invisible(
      lapply(grep(id, names(.input), value = TRUE), function(i) {
        .subset2(.input, "impl")$.values$remove(i)
      })
    )
  }


  box::use(shiny, cli, bs4Dash, glue, shinyAce, styler)
  shiny$moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      cli$cli_alert_info(ns("lmModel"))

      shiny$observeEvent(input$deleteButton, {
        shiny$removeUI(selector = paste0("#", paste0(id, "-lmModel")))
        shiny$removeUI(selector = paste0("#", ns("deleteButton")))
        remove_shiny_inputs(id, input)
      })


      output[["lmModel"]] <- shiny$renderUI({
        shiny$req(inputData)
        data <- inputData()

        PARAMCD <- data[[1]]$PARAMCD
        analysis <- data[[1]]$analysis

        bs4Dash$bs4Card(
          title = paste0(id, "-statsmod"),
          id = environment(ns)[["namespace"]],
          width = 12,
          shiny$selectizeInput(ns("statsGroupPARAMCD"),
            shiny$h4(glue$glue("PARAMCD")),
            choices = data[[1]]$PARAMCD,
            selected = data[[1]]$PARAMCD, multiple = TRUE
          ),
          shiny$numericInput(
            ns("nStatistics"), "n",
            min = -Inf, max = Inf, value = 2
          ),
          shiny$numericInput(
            ns("rStatistics"), "r",
            min = -Inf, max = Inf, value = 2
          ),
          shiny$numericInput(
            ns("diff_pospctStatistics"), "diff_pct",
            min = -Inf, max = Inf, value = 10
          ),
          shiny$numericInput(
            ns("diff_negpctStatistics"), "diff_pct",
            min = -Inf, max = Inf, value = 10
          ),
          bs4Dash$actionButton(
            ns("deleteButton"),
            "Remove",
            status = "warning"
          ),
          shiny$uiOutput(ns("flaggingCode"))
        )
      })


      output$flaggingCode <- shiny$renderUI({
        shiny$req(input$diff_negpctStatistics)
        nStatistics <- input$nStatistics
        rStatistics <- input$rStatistics
        diff_negpctStatistics <- input$diff_negpctStatistics

        code <-
          styler$style_text(
            glue$glue(
              "if ((abs(diff_pct)/100 * {nStatistics} > 2 & diff_pct < {diff_negpctStatistics}) & ( p_value<0.05 | r=={rStatistics})) flag = -1"
            )
          )

        shinyAce$aceEditor(
          outputId = ns("ace"),
          theme = "chaos",
          mode = "r",
          fontSize = 20,
          autoScrollEditorIntoView = TRUE,
          minLines = 5,
          maxLines = 30,
          value = code
        )
      })
    },
    session = parentSession
  )
}
