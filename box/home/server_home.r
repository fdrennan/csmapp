#' @export
server <- function(input, output, session) {
  box::use(
    .. / metadata / server_metadata, shiny, dplyr, fs, cli,
    glue, purrr, readr, haven, htmlTable, bs4Dash,
    .. / analysis / server_analysis, DT
  )

  box::use(.. / analysis / ui_analysis)
  box::use(.. / devop / server_devop)
  box::use(uuid, storr, jsonlite, stringr, tidyr, openxlsx)
  sessionId <- uuid$UUIDgenerate()

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

    output$tab2UI <- shiny$renderUI({
      metadata <- metadata()
      purrr$map(
        unique(metadata$analysis), function(x) {
          bs4Dash::box(
            title = shiny$h2(toupper(x), class = "text-center display-2"),
            width = 12,
            ui_analysis$ui(x, data)
          )
        }
      )
    })

    purrr$map(
      metadata$analysis,
      function(x) server_analysis$server(id = x, data, parentSession = session, sessionId)
    )

    bs4Dash$updatebs4TabItems(
      session = shiny::getDefaultReactiveDomain(),
      "sidebarMenu",
      selected = "tab2"
    )
    bs4Dash$updateControlbar("homeControlbar")
  })

  output$scoreboard <- DT$renderDT({
    scoreboard <- openxlsx$read.xlsx(getOption("base_config"), 3)
    scoreboard_names <- colnames(scoreboard)
    dt <- DT$datatable(scoreboard, options = list(
      pageLength = 50, scrollX = "700px"
    ))
    DT$formatStyle(dt, columns = names(scoreboard_names), color = "white")
  })

  shiny$observeEvent(input$updateReview, {
    out <- shiny$reactiveValuesToList(input)
    storr <- storr::storr_rds("storr")
    out <- lapply(getOption("analysis_filter"), function(analysisName) {
      tryCatch(
        {
          out <- storr$get(paste0(analysisName, "-", sessionId))
          out$analysis <- analysisName
          out
        },
        error = function(err) {
          shiny$showNotification(paste("No data supplied for", analysisName))
        }
      )
    })

    flags <- shiny$reactive({
      out <- purrr$keep(out, ~ length(.) > 1)

      out <- purrr$map_dfr(
        out, function(x) {
          x_names <- names(x)

          analysis <- x$analysis
          x <- x[grepl("^[0-9]{4}-", x_names)]
          x_names <- names(x)
          x_values <- purrr$map(x, list)
          data <- dplyr$tibble(
            input_names = x_names,
            input_value = x_values,
            analysis = analysis
          )
          out <- tidyr$separate(data, input_names, c("flag_id", "value_name"))
          out <- dplyr$filter(out, value_name %in% c("flagValue", "flagCode", "statsGroupPARAMCD"))

          split(
            out, out$flag_id
          ) |>
            purrr$map_dfr(
              function(y) {
                paramcd <- y |>
                  dplyr$filter(value_name == "statsGroupPARAMCD") |>
                  dplyr$pull(input_value) |>
                  unlist()
                flagCode <- y |>
                  dplyr$filter(
                    value_name == "flagCode"
                  ) |>
                  dplyr$pull(input_value) |>
                  unlist()
                flagValue <- y |>
                  dplyr$filter(
                    value_name == "flagValue"
                  ) |>
                  dplyr$pull(input_value) |>
                  unlist()

                flag_id <- unique(y$flag_id)
                analysis <- unique(y$analysis)

                dplyr$tibble(
                  analysis = analysis,
                  flag_id = flag_id,
                  paramcd = paramcd,
                  flagCode = flagCode,
                  flagValue = flagValue
                )
              }
            )
        }
      )
    })


    output$reviewOut <- DT$renderDT({
      shiny$req(flags())
      DT$datatable(flags())
    })
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
