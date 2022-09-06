

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
      cli$cli_alert_info(ns("lmModel"))

      shiny$observeEvent(input$deleteButton, {
        shiny$removeUI(selector = paste0("#", paste0(id, "-lmModel")))
        shiny$removeUI(selector = paste0("#", ns("deleteButton")))
        remove_shiny_inputs(id, input)
      })

      output[["lmModel"]] <- shiny$renderUI({
        card_number <- as.numeric(stringr$str_sub(id, -4L, -1L))
        card_name <- paste0("Flagging Criteria ", card_number)
        shiny$div(
          class = "my-3",
          bs4Dash$bs4Card(
            title = card_name,
            id = environment(ns)[["namespace"]],
            width = 12,
            footer = {
              shiny$div(
                class = "text=right",
                bs4Dash$actionButton(
                  ns("deleteButton"), "",
                  icon = shiny$icon("x"),
                  style = "height: 3rem;"
                )
              )
            },
            shiny$uiOutput(ns("statisticsSetup"))
          )
        )
      })

      output$statisticsSetup <- shiny$renderUI({
        shiny$req(inputData)
        data <- inputData()
        PARAMCD <- data[[1]]$PARAMCD
        analysis <- data[[1]]$analysis

        shiny$wellPanel(
          shiny$h3('Select PARAMCD'),
          shiny$selectizeInput(ns("statsGroupPARAMCD"),"",
                               # shiny$h4(glue$glue("Select PARAMCD")),
                               choices = data[[1]]$PARAMCD,
                               selected = data[[1]]$PARAMCD, multiple = TRUE
          ),
          shiny$tags$hr(),
          shiny$textInput(ns('variableName'), 'Variable Name'),
          shiny$div(class='text-right', bs4Dash$actionButton(ns("addVariable"), "Add variable")),
          shiny$tags$hr(),
          shiny$div(id=ns('variables')),
          shiny$textOutput(ns("flaggingCode"))
        )
      })

      shiny$observeEvent(input$addVariable, {
        shiny$insertUI(
          selector = paste0("#", ns("variables")),
          where = "beforeBegin",ui = {
            shiny$wellPanel(
              shiny$numericInput(ns(input$variableName), input$variableName, value = 0)
            )
          }
        )
      })

    },
    session = parentSession
  )
}
