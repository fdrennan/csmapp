#' @export
ui_preview <- function(id = "preview") {
  box::use(shiny)
  ns <- shiny$NS(id)
  shiny$uiOutput(ns("previewData"))
}

#' @export
server_preview <- function(id = "preview", previewData) {
  box::use(shiny, dplyr, purrr, htmlTable)
  shiny$moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      output$previewData <- shiny$renderUI({
        shiny$req(previewData())
        dataNames <- previewData()
        purrr$map(
          dataNames,
          function(x) {
            data <- x$data
            nrowData <- nrow(data)
            column_names <- x$column_names
            analysis <- x$analysis
            column_names <- column_names[column_names == toupper(column_names)]
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
                  class = "d-flex justify-content-center",
                  htmlTable$htmlTable(nMissing)
                )
              )
            } else {
              nMissing <- shiny$div()
            }

            if ("PARAMCD" %in% colnames(x$data)) {
              paramcd <- unique(x$data[, "PARAMCD"]$PARAMCD)
              paramcd <- shiny$inputPanel(
                shiny$selectizeInput(ns(paste0(analysis, "PARAMCD")),
                  "PARAMCD", paramcd, paramcd,
                  multiple = TRUE
                ),
                shiny$numericInput(ns(paste0(analysis, "splits")), "Splits", 1, min = 0, max = 3, step = 1)
              )
            } else {
              paramcd <- shiny$div()
            }

            selectId <- paste0(analysis, "columns")
            shiny$wellPanel(
              shiny$h4(toupper(analysis), class = "font-weight-bold"),
              shiny$selectizeInput(
                ns(selectId), "Columns", column_names, column_names,
                multiple = TRUE
              ),
              paramcd,
              nMissing
            )
          }
        )
      })


      out <- shiny$reactive({
        shiny$reactiveValuesToList(input)
      })
      out
    }
  )
}
