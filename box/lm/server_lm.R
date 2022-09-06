

#' @export
server <- function(id, parentSession, inputData) {
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
            bs4Dash$actionButton(
              ns("deleteButton"), "",
              icon = shiny$icon("x", class = "text-right")
              # style = "height: 3rem;"
            )
          },
          shiny$fluidRow(
            shiny$column(
              6,
              shiny$wellPanel(
                shiny$selectInput(ns("flagValue"), "Flag", choices = c(-1, 0, 1), selected = 1),
                shiny$selectizeInput(ns("statsGroupPARAMCD"), "PARAMCD",
                  choices = PARAMCD,
                  selected = PARAMCD, multiple = TRUE
                ),
                shiny$tags$hr(),
                shiny$textInput(ns("variableName"), label = NULL, placeholder = "Variable Name"),
                shiny$div(
                  class = "text-right",
                  bs4Dash$actionButton(
                    ns("addVariable"),
                    shiny$tags$em("Add variable")
                  )
                )
              )
            ),
            shiny$column(
              6, shiny$uiOutput(ns("statisticsSetup"))
            ),
            shiny$column(
              6, shiny$uiOutput(ns("flaggingTemplate"))
            ),
            shiny$column(
              6, shiny$uiOutput(ns("flaggingPreview"))
            )
          )
        )
      })


      output$statisticsSetup <- shiny$renderUI({
        shiny$req(inputData)
        data <- inputData()
        PARAMCD <- data[[1]]$PARAMCD
        analysis <- data[[1]]$analysis

        switch(analysis,
          "aei" = {
            currentFlag <- input$flag
            shiny$wellPanel(
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
                min = -Inf, max = Inf, value = -10
              ),
              shiny$div(id = ns("variables"))
            )
          }
        )
      })

      output$flaggingTemplate <- shiny$renderUI({
        # shiny$req(input$textTemplate)
        flag_crit <- "if ((abs(diff_pct)/100*n>{n} & diff_pct<{diff_pct}) & ( p_value<0.05 | r=={r})) {flag= -1};"
        shinyAce$aceEditor(outputId = ns('flagInput'), value = flag_crit, theme = 'chaos',
                           fontSize = 18, wordWrap = TRUE, mode = 'r', autoComplete = 'enabled')
      })

      output$flaggingPreview <- shiny$renderUI({
        shiny$req(input$flagInput)
        shiny$debounce(input$flagInput, 100)
        shinyAce$aceEditor(outputId = ns('flag'), value = input$flagInput, theme = 'chaos')
      })

      shiny$observeEvent(input$addVariable, {
        if (input$variableName == "") {
          shiny$showNotification("No variable name supplied", type = "warning")
        }
        shiny$req(input$variableName)
        shiny$insertUI(
          selector = paste0("#", ns("variables")),
          where = "beforeBegin", ui = {
            shiny$numericInput(ns(input$variableName), input$variableName, value = 0)
          }
        )
      })
    },
    session = parentSession
  )
}
