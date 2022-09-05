#' @export
server <- function(input, output, session) {
  box::use(
    .. / metadata / server_metadata, shiny, dplyr, fs, cli,
    glue, purrr, readr, haven, htmlTable, bs4Dash,
    .. / analysis / server_analysis
  )
  box::use(.. / analysis / ui_analysis)
  box::use(.. / devop / server_devop)
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
      bs4Dash$box(
        title = "Review",
        width = 12,
        purrr$map(
          unique(metadata$analysis), ui_analysis$ui
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
