#' @export
server <- function(input, output, session) {
  box::use(
    .. / metadata / server_metadata, shiny, dplyr, fs, cli,
    glue, purrr, readr, haven, htmlTable, bs4Dash,
    .. / analysis / server_analysis
  )
  #
  box::use(.. / analysis / ui_analysis)
  box::use(.. / devop / server_devop)



  ns <- session$ns
  server_devop$server()
  metadata <- server_metadata$server()

  dataToAnalyze <- shiny$reactive({
    box::use(.. / processing)
    shiny$req(metadata())
    data <- processing$make_data_to_analyze(metadata())
    data
  })


  shiny$observe({
    shiny$req(dataToAnalyze())
    metadata <- metadata()
    data <- dataToAnalyze()



    output$setupAnalysis <- shiny$renderUI({

      # CompareProportion	T_Zscore	1.68	1.8
      # TukeyOutliers	fence	outer	enter choice of "outer" or "inner"
      # DosingAnalysis	cutoff_perplanned	80	90
      # CompareProportion	min_n_value	2
      # CompareProportion	min_n_number_betabinom	5


      shiny$fluidRow(
        bs4Dash$box(title='Program Configuration Parameters',
          width = 12,
          shiny$wellPanel(
            shiny$h4("Compare Proportion"),
            shiny$numericInput(ns("T_Zscore"), "T_Zscore", min = -Inf, max = Inf, value = 1.68),
            shiny$numericInput(ns("min_n_number_betabinom"), "min_n_number_betabinom", min = -Inf, max = Inf, value = 5),
            shiny$numericInput(ns("min_n_value"), "min_n_value", min = -Inf, max = Inf, value = 2),
            shiny$h4("Tukey"),
            shiny$selectInput(ns("TukeyOutliers"), "TukeyOutliers", choices = c("inner", "output"), selected = "outer"),
            shiny$h4("Dosing Analysis"),
            shiny$numericInput(ns("cutoff_perplanned"), "cutoff_perplanned", min = -Inf, max = Inf, value = 80)
          )
        ),
        purrr$map(
          unique(metadata$analysis), function(x) {
            bs4Dash::box(title = toupper(x), width = 12, ui_analysis$ui(x, data))
          }
        )
      )
    })

    purrr$walk(
      metadata$analysis,
      function(x) server_analysis$server(id = x, data, parentSession = session)
    )
  })
}



# todo <- function() {
#   nrowData <- nrow(data)
#
#   nMissing <- data |>
#     dplyr$summarise_all(function(x) {
#       sum(is.na(x)) / nrowData
#     })
#
#   nMissing <-
#     nMissing |>
#     dplyr$select_if(
#       function(x) {
#         x > 0
#       }
#     )
#   if (ncol(nMissing)) {
#     nMissing <- nMissing |>
#       dplyr$mutate_all(function(x) {
#         paste0(
#           round(x, 2), "%"
#         )
#       })
#     colnames(nMissing) <- paste0(" - ", colnames(nMissing), " - ")
#     nMissing <- shiny$div(
#       shiny$h4("Missing Data"),
#       shiny$div(
#         htmlTable$htmlTable(nMissing)
#       )
#     )
#   } else {
#     nMissing <- shiny$div()
#   }
#   if ("PARAMCD" %in% colnames(x$data)) {
#     paramcd <- unique(x$data[, "PARAMCD"]$PARAMCD)
#     paramcd <- shiny$div(
#       shiny$h3("PARAMCD"),
#       purrr$map(paramcd, function(x) {
#         shiny$tags$em(paste0(x, " * "))
#       })
#     )
#   } else {
#     paramcd <- shiny$div()
#   }
# }
