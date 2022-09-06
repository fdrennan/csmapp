

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
        card_number <- as.numeric(stringr$str_sub(id, -4L, -1L))
        card_name <- paste0("Flagging Criteria ", card_number)
        bs4Dash$bs4Card(
          title = shiny$h2(card_name),
          id = environment(ns)[["namespace"]],
          width = 12,
          footer = {
            bs4Dash$actionButton(
              ns("deleteButton"), "",
              icon = shiny$icon("x", class = "text-right")
              # style = "height: 3rem;"
            )
          },
          shiny$uiOutput(ns("statisticsSetup"))
        )
      })

      output$statisticsSetup <- shiny$renderUI({
        shiny$req(inputData)
        data <- inputData()
        PARAMCD <- data[[1]]$PARAMCD
        analysis <- data[[1]]$analysis

        shiny$fluidRow(
          shiny$column(
            12,
            shiny$h3("Select PARAMCD"),
            shiny$selectizeInput(ns("statsGroupPARAMCD"), "",
                                 choices = data[[1]]$PARAMCD,
                                 selected = data[[1]]$PARAMCD, multiple = TRUE
            )
          ),
          shiny$column(
            12,
            shiny$h3('Standard Variables')
          ), 
          shiny$column(
            12,
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
            )
          ),
          shiny$tags$hr(),
          shiny$column(
            12,
            shiny$h3('Add Variables'),
            shiny$textInput(ns("variableName"), "Variable Name"),
            shiny$div(class = "text-right", bs4Dash$actionButton(ns("addVariable"), "Add variable")),
            shiny$div(id = ns("variables"))
          ),
          shiny$tags$hr(),
          shiny$column(
            12,
            shiny$textOutput(ns("flaggingCode"))
          )
        )
      })

      shiny$observeEvent(input$addVariable, {
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
