#' @export
server <- function(id = "analysis") {
  box::use(shiny, bs4Dash, cli)


      # output$previewData <- shiny$renderUI({
      #   shiny$req(dataToAnalyze)
      #   data <- dataToAnalyze()
      #
      #   browser()
      #   purrr$map(
      #     data,
      #     function(x) {
      #       data <- x$data
      #       bs4Dash$bs4Card(
      #         width = 12,
      #         id = paste0("analysisSetup", x$analysis),
      #         title = toupper(x$analysis),
      #         closable = TRUE,
      #         sidebar = bs4Dash$cardSidebar(
      #           id = paste0("analysisSetup", x$analysis, "sidebar"),
      #           startOpen = TRUE,
      #           easyClose = TRUE,
      #           shiny$numericInput(paste0(x$analysis, "statsSplit"), "Stats Split",
      #                              value = 1,
      #                              min = 1, max = 10, step = 1
      #           )
      #         ),
      #         shiny$uiOutput(paste0("statsSplitUI", x$analysis))
      #       )
      #     }
      #   )
      # })
      #
      #
      # output$statsSplitUIaei <- shiny$renderUI({
      #   shiny$req(input$aeistatsSplit)
      #   data <- dataToAnalyze()
      #   data <- purrr$keep(data, function(x) {
      #     x$analysis %in% c("aei", "rgv")
      #   })
      #   purrr$map(
      #     1:input$aeistatsSplit,
      #     function(x) {
      #       bs4Dash$box(
      #         width = 12,
      #         shiny$selectizeInput(paste0('aei', x, "statsGroupPARAMCD"),
      #                              glue$glue("Stats Group {x}"),
      #                              choices = data[[1]]$PARAMCD,
      #                              selected = data[[1]]$PARAMCD, multiple = TRUE
      #         ),
      #         shiny$fluidRow(
      #           shiny$column(
      #             6,
      #             shiny$numericInput(paste0('aei', x, "nStatistics"),
      #                                "n",
      #                                min = -Inf, max = Inf, value = 2
      #             )
      #           ),
      #           shiny$column(
      #             6,
      #             shiny$numericInput(paste0('aei', x, "rStatistics"),
      #                                "r",
      #                                min = -Inf, max = Inf, value = 2
      #             )
      #           ),
      #           shiny$column(
      #             6,
      #             shiny$numericInput(paste0('aei', x, "diff_pctStatistics"),
      #                                "diff_pct",
      #                                min = -Inf, max = Inf, value = 10
      #             )
      #           )
      #         )
      #       )
      #     }
      #   )
      # })
    }
  )
}
