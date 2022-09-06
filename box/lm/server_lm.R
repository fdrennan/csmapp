

#' @export
server <- function(id, parentSession, inputData) {
  analysis_flagging <- function(analysis) {
    switch(analysis,
      "aei" = {
        inputs <- shiny$fluidRow(
          class = "d-flex justify-content-around",
          shiny$numericInput(
            ns("n"), "n",
            min = -Inf, max = Inf, value = 2
          ),
          shiny$numericInput(
            ns("r"), "r",
            min = -Inf, max = Inf, value = 2
          ),
          shiny$numericInput(
            ns("diff_pct"), "diff_pct",
            min = -Inf, max = Inf,
            value = ifelse(input$flagValue == -1, -10, 10)
          ),
          shiny$div(id = ns("variables"))
        )

        if (input$flagValue == -1) {
          flag_crit <- c(
            "var_1 <- abs(diff_pct)/100*n>{n}",
            "var_2 <- diff_pct<{diff_pct}",
            "var_3 <- p_value < 0.05  | r=={r}",
            "all(var_1 & var_2, var_3);"
          )
        } else {
          flag_crit <- c(
            "var_1 <- abs(diff_pct)/100*n>{n}",
            "var_2 <- diff_pct>{diff_pct}",
            "var_3 <- p_value < 0.05  | r=={r}",
            "all(var_1 & var_2, var_3);"
          )
        }

        list(
          inputs = inputs,
          flag_crit = paste0(flag_crit, sep = "\n")
        )
      }
    )
  }

  remove_shiny_inputs <- function(id, .input) {
    invisible(
      lapply(grep(id, names(.input), value = TRUE), function(i) {
        .subset2(.input, "impl")$.values$remove(i)
      })
    )
  }

  box::use(shiny, cli, bs4Dash, glue, shinyAce, stringr, styler)

  shiny$moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      shiny$observeEvent(input$deleteButton, {
        shiny$removeUI(selector = paste0("#", paste0(id, "-lmModel")))
        shiny$removeUI(selector = paste0("#", ns("deleteButton")))
        remove_shiny_inputs(id, input)
      })

      output$lmModel <- shiny$renderUI({
        shiny$req(inputData)
        card_number <- as.numeric(stringr$str_sub(id, -4L, -1L))
        card_name <- paste0("Flagging Criteria ", card_number)
        data <- inputData()
        PARAMCD <- data[[1]]$PARAMCD
        analysis <- data[[1]]$analysis
        bs4Dash$bs4Card(
          title = shiny$h3(card_name),
          id = environment(ns)[["namespace"]],
          width = 12,
          footer = {
            shiny$div(
              class = "text-center",
              bs4Dash$actionButton(
                ns("deleteButton"), "",
                icon = shiny$icon("x")
                # style = "height: 3rem;"
              )
            )
          },
          shiny$fluidRow(
            shiny$column(
              6,
              offset = 3,
              shiny$wellPanel(
                shiny$selectizeInput(ns("statsGroupPARAMCD"), "PARAMCD",
                  choices = PARAMCD,
                  selected = PARAMCD, multiple = TRUE
                ),
                shiny$selectInput(
                  ns("flagValue"), "Flag",
                  choices = c(-1, 0, 1), selected = 1
                )
              )
            ),
            shiny$column(12, shiny$uiOutput(ns("statisticsSetup"))),
            shiny$column(6, shiny$uiOutput(ns("flaggingTemplate"))),
            shiny$column(6, shiny$uiOutput(ns("flaggingPreview")))
          )
        )
      })


      flaggingFilter <- shiny$reactive({
        shiny$req(inputData())
        data <- inputData()
        analysis <- data[[1]]$analysis
        analysis_flagging(analysis)
      })

      output$statisticsSetup <- shiny$renderUI({
        shiny$req(input$flagValue)
        shiny$req(flaggingFilter())
        data <- inputData()
        PARAMCD <- data[[1]]$PARAMCD
        analysis <- data[[1]]$analysis
        switch(analysis,
          "aei" = flaggingFilter()$inputs
        )
      })

      output$flaggingTemplate <- shiny$renderUI({
        shiny$req(flaggingFilter())
        shiny$div(
          shinyAce$aceEditor(
            outputId = ns("flagInput"), value = flaggingFilter()$flag_crit, theme = "chaos",
            fontSize = 14, wordWrap = TRUE, autoComplete = "enabled",
            minLines = 1, maxLines = 5, height = "130px"
          )
        )
      })

      gluedFlagData <- shiny$reactive({
        shiny$req(input$flagInput)
        flagInput <- with(
          shiny$reactiveValuesToList(input),
          glue$glue(input$flagInput)
        )
      })

      output$flaggingPreview <- shiny$renderUI({
        shiny$req(gluedFlagData())
        flagInput <- gluedFlagData()
        shinyAce$aceEditor(
          outputId = ns("flag"), value = flagInput, theme = "chaos",
          fontSize = 14, wordWrap = TRUE, mode = "r", minLines = 1, maxLines = 5, height = "130px"
        )
      })
    },
    session = parentSession
  )
}
