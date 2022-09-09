

#' @export
server <- function(id, parentSession, inputData) {
  remove_shiny_inputs <- function(id, .input) {
    invisible(
      lapply(grep(id, names(.input), value = TRUE), function(i) {
        .subset2(.input, "impl")$.values$remove(i)
      })
    )
  }

  box::use(
    shiny, cli, bs4Dash, glue, .. / flagging / utils,
    shinyAce, stringr, styler, dplyr, openxlsx
  )
  
  
  
  shiny$moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      shiny$observeEvent(input$deleteButton, {
        names_of_inputs <- names(shiny$reactiveValuesToList(input))
        shiny$removeUI(selector = paste0("#", paste0(id, "-lmModel")))
        shiny$removeUI(selector = paste0("#", ns("deleteButton")))
        lapply(
          names_of_inputs, function(x) {
            remove_shiny_inputs(x, input)
          }
        )
      })

      output$lmModel <- shiny$renderUI({
        shiny$req(inputData)
        data <- inputData()
        flagging_setup <- openxlsx$read.xlsx(getOption('base_config'), 2)
        analysis <- data[[1]]$analysis
        id_step <- stringr$str_extract(id, '[0-9]{4}$')
        flag <- unique(flagging_setup$flag)[as.numeric(id_step)]
        flag_setup <- dplyr$filter(
          flagging_setup, 
          analysis == !!analysis,
          flag == !!flag 
        )
        PARAMCD_file <- strsplit(flag_setup$paramcd[1], ', ', )[[1]]
        
        card_name <- paste("Flag", id_step)
        
        PARAMCD_data <- data[[1]]$PARAMCD
        bs4Dash$bs4Card(
          title = shiny$h3(card_name),
          id = environment(ns)[["namespace"]],
          width = 12,
          footer = {
            shiny$div(class = "text-right", bs4Dash$actionButton(
              ns("deleteButton"), "",
              icon = shiny$icon("x")
            ))
          },
          shiny$fluidRow(
            shiny$column(
              12,
              shiny$wellPanel(
                shiny$selectizeInput(
                  ns("flagValue"), "Flag",
                  choices = unique(c(flagging_setup$flag, 2, 3, 4)), 
                  selected = flag
                ),
                shiny$selectizeInput(ns("statsGroupPARAMCD"), "PARAMCD",
                  choices = PARAMCD_data,
                  selected = toupper(PARAMCD_file),
                  multiple = TRUE
                )
              )
            ),
            shiny$uiOutput(ns("statisticsSetup"), container = function(...) {
              shiny$column(12, ...)
            }),
            shiny$column(
              12,
              shiny$h3("Flagging Template"),
              shiny$uiOutput(ns("flaggingTemplate"), container = function(...) {
                shiny$column(12, ...)
              }),
              shiny$h3("Verify Flagging Criteria"), shiny$uiOutput(ns("flaggingPreview"), container = function(...) {
                shiny$column(12, ...)
              })
            ),
            shiny$column(12, shiny$div(
              class = "text-right", bs4Dash$actionButton(ns("updateStats"), "Verify Statistics")
            ))
          )
        )
      })


      flaggingFilter <- shiny$reactive({
        shiny$req(inputData())
        data <- inputData()
        analysis <- data[[1]]$analysis
        utils$analysis_flagging(analysis, ns, input)
      })

      output$statisticsSetup <- shiny$renderUI({
        shiny$req(flaggingFilter())
        flaggingFilter()$inputs
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

      gluedFlagData <- shiny$eventReactive(input$updateStats, {
        flagInput <- with(
          shiny$reactiveValuesToList(input),
          styler$style_text(glue$glue(input$flagInput))
        )
      })

      output$flaggingPreview <- shiny$renderUI({
        shiny$req(gluedFlagData())
        flagInput <- gluedFlagData()
        shinyAce$aceEditor(
          outputId = ns("flagCode"), value = flagInput, theme = "chaos",
          highlightActiveLine = TRUE,
          fontSize = 14, wordWrap = TRUE,
          mode = "r", minLines = 1, maxLines = 15,
          height = "130px"
        )
      })
    },
    session = parentSession
  )
}
