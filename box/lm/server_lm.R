

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

        shiny$div(
          class='my-3',
          shiny$div(
            class='d-flex justify-content-end my-3',
            bs4Dash$actionButton(
              ns("deleteButton"),"",
              icon = shiny$icon("x"),
              # class = "btn", 
              style = "height: 3rem;"
            )
          ),
          bs4Dash$bs4Card(
            title = id,
            id = environment(ns)[["namespace"]],
            width = 12,
            shiny$wellPanel(
              shiny$selectizeInput(ns("statsGroupPARAMCD"),
                                   shiny$h4(glue$glue("Signal / Flag Mapper")),
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
                ns("diff_pctStatistics"), "diff_pct",
                min = -Inf, max = Inf, value = 10
              ),
              shiny$numericInput(
                ns("flagValue"), "Flag",
                min = -5, max = 5, value = 1, step = 1
              ),
              shiny$uiOutput(ns("flaggingCode"))
            )
          )
        )
      })

      syledCode <- shiny$reactive({
        shiny$req(input$diff_pctStatistics)

        nStatistics <- input$nStatistics
        diff_pctStatistics <- input$diff_pctStatistics
        rStatistics <- input$rStatistics

        stats_1 <- glue$glue("(abs(diff_pct)/100 * {nStatistics} > 2)")
        stats_2 <- glue$glue("(diff_pct < {diff_pctStatistics})")
        stats_3 <- glue$glue("(p_value<0.05 | r== {rStatistics})")
        stats_code <- paste0(c(stats_1, stats_2, stats_3), collapse = " &\n")
        # 
        code <-
          styler$style_text(
            with(
              input,
              glue$glue(
                "if ({stats_code}) flag = {input$flagValue}"
              )
            )
          )
      })

      
      output$flaggingCode <- shiny$renderUI({
        shiny$req(syledCode())
        code <- syledCode()
        shinyAce$aceEditor(
          outputId = ns("ace"),
          theme = "chaos",
          mode = "r",
          fontSize = 18,
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
