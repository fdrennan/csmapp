#' @export
server <- function(input, output, session) {
  box::use(
    .. / metadata / server_metadata, shiny, dplyr, fs, cli,
    glue, purrr, readr, haven, htmlTable, bs4Dash
  )
  box::use(.. / devop / server_devop)
  server_devop$server()
  filteredData <- server_metadata$server()

  dataToAnalyze <- shiny$reactive({
    box::use(.. / processing)
    shiny$req(filteredData())
    data <- processing$make_data_to_analyze(filteredData())
    data
  })

  output$previewData <- shiny$renderUI({
    shiny$req(dataToAnalyze)
    data <- dataToAnalyze()

    purrr$map(
      data,
      function(x) {
        data <- x$data
        bs4Dash$bs4Card(
          width = 12,
          id = paste0("analysisSetup", x$analysis),
          title = toupper(x$analysis),
          closable = TRUE,
          sidebar = bs4Dash$cardSidebar(
            id = paste0("analysisSetup", x$analysis, "sidebar"),
            startOpen = TRUE,
            easyClose = TRUE,
            shiny$numericInput(paste0(x$analysis, "statsSplit"), "Stats Split",
              value = 1,
              min = 1, max = 10, step = 1
            )
          ),
          shiny$uiOutput(paste0("statsSplitUI", x$analysis))
        )
      }
    )
  })


  output$statsSplitUIaei <- shiny$renderUI({
    shiny$req(input$aeistatsSplit)
    data <- dataToAnalyze()
    data <- purrr$keep(data, function(x) {
      x$analysis %in% c("aei", "rgv")
    })
    purrr$map(
      1:input$aeistatsSplit,
      function(x) {
        bs4Dash$box(
          width = 12,
          shiny$selectizeInput(paste0('aei', x, "statsGroupPARAMCD"),
                               glue$glue("Stats Group {x}"),
                               choices = data[[1]]$PARAMCD,
                               selected = data[[1]]$PARAMCD, multiple = TRUE
          ),
          shiny$fluidRow(
            shiny$column(
              6,
              shiny$numericInput(paste0('aei', x, "nStatistics"),
                                 "n",
                                 min = -Inf, max = Inf, value = 2
              )
            ),
            shiny$column(
              6,
              shiny$numericInput(paste0('aei', x, "rStatistics"),
                                 "r",
                                 min = -Inf, max = Inf, value = 2
              )
            ),
            shiny$column(
              6,
              shiny$numericInput(paste0('aei', x, "diff_pctStatistics"),
                                 "diff_pct",
                                 min = -Inf, max = Inf, value = 10
              )
            )
          )
        )
      }
    )
  })
}



todo <- function() {
  nrowData <- nrow(data)

  nMissing <- data |>
    dplyr$summarise_all(function(x) {
      sum(is.na(x)) / nrowData
    })

  nMissing <-
    nMissing |>
    dplyr$select_if(
      function(x) {
        x > 0
      }
    )
  if (ncol(nMissing)) {
    nMissing <- nMissing |>
      dplyr$mutate_all(function(x) {
        paste0(
          round(x, 2), "%"
        )
      })
    colnames(nMissing) <- paste0(" - ", colnames(nMissing), " - ")
    nMissing <- shiny$div(
      shiny$h4("Missing Data"),
      shiny$div(
        htmlTable$htmlTable(nMissing)
      )
    )
  } else {
    nMissing <- shiny$div()
  }
  if ("PARAMCD" %in% colnames(x$data)) {
    paramcd <- unique(x$data[, "PARAMCD"]$PARAMCD)
    paramcd <- shiny$div(
      shiny$h3("PARAMCD"),
      purrr$map(paramcd, function(x) {
        shiny$tags$em(paste0(x, " * "))
      })
    )
  } else {
    paramcd <- shiny$div()
  }
}
