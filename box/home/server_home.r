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
    processing$make_data_to_analyze(filteredData())
  })

  output$previewData <- shiny$renderUI({
    shiny$req(dataToAnalyze)
    dataNames <- dataToAnalyze()

    purrr$map(
      dataNames,
      function(x) {
        # For each analysis, grab the file
        data <- x$data
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
        bs4Dash$bs4Card(width=12,
          title = toupper(x$analysis), closable=TRUE,
          paramcd,
          nMissing
        )
      }
    )
  })
}
