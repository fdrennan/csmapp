

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
                class = "d-flex justify-content-end",
                bs4Dash$actionButton(
                  ns("deleteButton"), "",
                  icon = shiny$icon("x"),
                  # class = "btn",
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
          shiny$selectizeInput(ns("statsGroupPARAMCD"),
            shiny$h4(glue$glue("Signal / Flag Mapper")),
            choices = data[[1]]$PARAMCD,
            selected = data[[1]]$PARAMCD, multiple = TRUE
          ),
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
            min = -Inf, max = Inf, value = 10
          ),
          # shiny$div(id = ns("additionalStatistics")),
          shiny$actionButton(ns("addVariable"), "Add variable"),
          shiny$numericInput(
            ns("flag"), "Flag",
            min = -5, max = 5, value = 1, step = 1
          ),
          shiny$textOutput(ns("flaggingCode"))
        )
      })

      shiny$observeEvent(input$addVariable, {
        inputId <- paste0("numericInput", input$addVariable)
        shiny$insertUI(
          selector = paste0("#", ns("addVariable")),
          where = "beforeBegin",
          shiny$numericInput(ns(inputId), inputId, value = 0)
        )
      })


      # output$flaggingCode <- shiny$renderText({
      #   shiny$req(syledCode())
      #   code <- syledCode()
      #   code
      # })
    },
    session = parentSession
  )
}
