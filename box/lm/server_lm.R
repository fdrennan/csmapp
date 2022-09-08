

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
    shiny, cli, bs4Dash, glue,
    shinyAce, stringr, styler, dplyr
  )

  shiny$moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      analysis_flagging <- function(analysis) {
        out <- switch(analysis,
          "aei" = {
            inputs <- list(
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
                min = -Inf, max = Inf,
                value = ifelse(input$flagValue == -1, -10, 10)
              ),
              shiny$div(id = ns("variables"))
            )

            if (input$flagValue == -1) {
              flag_crit <- c(
                "var_1 <- abs(diff_pct)/100*n>{n}",
                "var_2 <- diff_pct<{diff_pct}",
                "var_3 <- p_value < 0.05  | r=={r}",
                "all(var_1 & var_2, var_3);"
              )
            } else {
              flag_crit <- c(
                "var_1 <- abs(diff_pct)/100*n>{n}",
                "var_2 <- diff_pct>{diff_pct}",
                "var_3 <- p_value < 0.05  | r=={r}",
                "all(var_1 & var_2, var_3);"
              )
            }

            list(
              inputs = inputs,
              flag_crit = paste0(flag_crit, sep = "\n")
            )
          },
          "rgv" = {
            inputs <- list(
              shiny$numericInput(
                ns("site_pct"), "site_pct",
                min = -Inf, max = Inf, value = 25
              ),
              shiny$numericInput(
                ns("diff_pct"), "diff_pct",
                min = -Inf, max = Inf, value = 10
              ),
              shiny$numericInput(
                ns("r"), "r",
                min = -Inf, max = Inf, value = 3
              ),
              shiny$numericInput(
                ns("p_value"), "p_value",
                min = -Inf, max = Inf, value = .05
              ),
              shiny$div(id = ns("variables"))
            )

            flag_crit <-
              switch(input$flagValue,
                "1" = {
                  c(
                    "var_1 <- site_pct > {site_pct}",
                    "var_2 <- diff_pct > {diff_pct}",
                    "var_3 <- r > {r}",
                    "var_4 <-   p_value < {p_value}",
                    "all(var_1, var_2, var_3, var_4);"
                  )
                }
              )

            list(
              inputs = inputs,
              flag_crit = paste0(flag_crit, sep = "\n")
            )
          },
          "aecnt" = {
            inputs <- list(
              shiny$numericInput(
                ns("site_cnt"), "site_cnt",
                min = -Inf, max = Inf, value = 3
              ),
              shiny$numericInput(
                ns("diff_cnt"), "diff_cnt",
                min = -Inf, max = Inf, value = dplyr$case_when(
                  input$flagValue == 1 ~ 2,
                  input$flagValue == -1 ~ -2,
                  TRUE ~ 0
                )
              ),
              shiny$numericInput(
                ns("foldchange"), "foldchange",
                min = -Inf, max = Inf, value = dplyr$case_when(
                  input$flagValue == 1 ~ 2.5,
                  input$flagValue == -1 ~ -2.5,
                  TRUE ~ 0
                )
              ),
              shiny$numericInput(
                ns("adjusted_p_value"), "adjusted_p_value",
                min = -Inf, max = Inf, value = .05
              ),
              shiny$div(id = ns("variables"))
            )

            flag_crit <-
              switch(input$flagValue,
                "1" = {
                  c(
                    "var_1 <- site_cnt > {site_cnt}",
                    "var_2 <- adjusted_p_value < {adjusted_p_value}",
                    "var_3 <- diff_cnt > {diff_cnt}",
                    "var_4 <-   foldchange > {foldchange}",
                    "all(var_1, var_2, var_3, var_4);"
                  )
                },
                "-1" = {
                  c(
                    "var_2 <- adjusted_p_value < {adjusted_p_value}",
                    "var_3 <- diff_cnt < {diff_cnt}",
                    "var_4 <-   foldchange < {foldchange}",
                    "all(var_1, var_2, var_3, var_4);"
                  )
                }
              )

            list(
              inputs = inputs,
              flag_crit = paste0(flag_crit, sep = "\n")
            )
          },
          "aegap" = {
            inputs <- list(
              shiny$numericInput(
                ns("n"), "n",
                min = -Inf, max = Inf, value = 1
              ),
              shiny$numericInput(
                ns("diff_avg"), "diff_avg",
                min = -Inf, max = Inf, value = 100
              ),
              shiny$numericInput(
                ns("p_value"), "p_value",
                min = -Inf, max = Inf, value = 0.05
              ),
              shiny$div(id = ns("variables"))
            )

            flag_crit <- c(
              "var_1 <- diff_avg < {diff_avg}",
              "var_2 <- p_value < {p_value}",
              "var_3 <-   n > {n}",
              "all(var_1, var_2, var_3);"
            )

            list(
              inputs = inputs,
              flag_crit = paste0(flag_crit, sep = "\n")
            )
          },
          "rgm" = {
            inputs <- list(
              shiny$numericInput(
                ns("n"), "n",
                min = -Inf, max = Inf, value = 5
              ),
              shiny$numericInput(
                ns("r"), "r",
                min = -Inf, max = Inf, value = 3
              ),
              shiny$numericInput(
                ns("site_pct"), "site_pct",
                min = -Inf, max = Inf, value = 30
              ),
              shiny$numericInput(
                ns("diff_pct"), "diff_pct",
                min = -Inf, max = Inf, value = 20
              ),
              shiny$numericInput(
                ns("p_value"), "p_value",
                min = -Inf, max = Inf, value = 0.05
              ),
              shiny$numericInput(
                ns("site_max_diff_pct"), "site_max_diff_pct",
                min = -Inf, max = Inf, value = 35
              ),
              shiny$numericInput(
                ns("sum_site_top2_diff_pct"), "sum_site_top2_diff_pct",
                min = -Inf, max = Inf, value = 55
              ),
              shiny$numericInput(
                ns("sum_site_top3_diff_pct"), "sum_site_top3_diff_pct",
                min = -Inf, max = Inf, value = 75
              ),
              shiny$div(id = ns("variables"))
            )

            flag_crit <- c(
              "var_1 <- n >= {n}",
              "var_2 <- r >= {r}",
              "var_3 <- site_pct > {site_pct}",
              "var_4 <- diff_pct > diff_pct",
              "var_5 <- p_value < {p_value}",
              "var_6 <- site_max_diff_pct > 35",
              "var_7 <- sum_site_top2_diff_pct > {sum_site_top2_diff_pct}",
              "var_8 <- sum_site_top3_diff_pct > {sum_site_top3_diff_pct}",
              "all(\nvar_1, var_2, var_3, var_4, var_5, all(var_6, var_7, var_8));"
            )

            list(
              inputs = inputs,
              flag_crit = paste0(flag_crit, sep = "\n")
            )
          },
          "underdose" = {
            inputs <- list(
              shiny$numericInput(
                ns("diff_pct"), "diff_pct",
                min = -Inf, max = Inf, value = 10
              ),
              shiny$numericInput(
                ns("p_value"), "p_value",
                min = -Inf, max = Inf, value = .05
              ),
              shiny$div(id = ns("variables"))
            )

            flag_crit <- c(
              "var_1 <- diff_pct > {diff_pct}",
              "var_2 <- p_value < {p_value}",
              "all(var_1, var_2)"
            )

            list(
              inputs = inputs,
              flag_crit = paste0(flag_crit, sep = "\n")
            )
          },
          "vitals" = {
            inputs <- list(
              shiny$numericInput(
                ns("site_value_cnt"), "site_value_cnt",
                min = -Inf, max = Inf, value = 8
              ),
              shiny$numericInput(
                ns("oddsRatio"), "oddsRatio",
                min = -Inf, max = Inf, value = 4
              ),
              shiny$numericInput(
                ns("site_value_pct"), "site_value_pct",
                min = -Inf, max = Inf, value = 15
              ),
              shiny$numericInput(
                ns("diff_pct"), "diff_pct",
                min = -Inf, max = Inf, value = 10
              ),
              shiny$numericInput(
                ns("p_value"), "p_value",
                min = -Inf, max = Inf, value = 10
              ),
              shiny$div(id = ns("variables"))
            )

            flag_crit <- c(
              "var_1 <- p_value < {p_value}",
              "var_2 <- site_value_cnt > {site_value_cnt}",
              "var_3 <- oddsRatio > {oddsRatio}",
              "var_4 <- site_value_pct > {site_value_pct}",
              "var_5 <- diff_pct > {diff_pct}",
              "all(var_1, var_2, var_3, var_4, var_5)"
            )

            list(
              inputs = inputs,
              flag_crit = paste0(flag_crit, sep = "\n")
            )
          }
        )
        out$inputs <- shiny$fluidRow(
          lapply(out$inputs, function(x) x)
        )
        out
      }

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
        card_number <- as.numeric(stringr$str_sub(id, -4L, -1L))
        card_name <- paste("Flag", card_number)
        data <- inputData()
        PARAMCD <- data[[1]]$PARAMCD
        analysis <- data[[1]]$analysis
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
            shiny$div(class='d-flex justify-content-around', shiny$selectInput(
              ns("flagValue"), "Flag",
              choices = c(-1, 0, 1), selected = 1
            ),
            shiny$selectizeInput(ns("statsGroupPARAMCD"), "PARAMCD",
                                 choices = PARAMCD,
                                 selected = PARAMCD, multiple = TRUE
            )),
            shiny$column(12, shiny$uiOutput(ns("statisticsSetup"))),
            shiny$column(
              12,
              shiny$h3("Flagging Template"),
              shiny$uiOutput(ns("flaggingTemplate")),
              shiny$h3("Verify Flagging Criteria"), shiny$uiOutput(ns("flaggingPreview"))
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
        analysis_flagging(analysis)
      })

      output$statisticsSetup <- shiny$renderUI({
        shiny$req(input$flagValue)
        shiny$req(flaggingFilter())
        data <- inputData()
        PARAMCD <- data[[1]]$PARAMCD
        analysis <- data[[1]]$analysis
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
