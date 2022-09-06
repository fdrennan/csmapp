

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
          shiny$selectInput(ns("flagValue"), "Flag", choices = c(-1, 0, 1), selected = 1),
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
            6,
            shiny$wellPanel(
              shiny$h3("Select PARAMCD"),
              shiny$selectizeInput(ns("statsGroupPARAMCD"), "",
                choices = data[[1]]$PARAMCD,
                selected = data[[1]]$PARAMCD, multiple = TRUE
              )
            )
          ),
          shiny$column(
            6,
            shiny$h3("Statistics"),
            switch(analysis,
              "aei" = {
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
                    min = -Inf, max = Inf, value = 10
                  ),
                  shiny$div(id = ns("variables")),
                  shiny$textInput(ns("variableName"), label = NULL, placeholder = "Variable Name"),
                  shiny$div(
                    class = "text-right",
                    bs4Dash$actionButton(
                      ns("addVariable"),
                      shiny$tags$em("Add variable")
                    )
                  )
                )
              }
            )
          )
        )
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
