if (FALSE) {
  library(shiny)
  library(cli)
  data(mtcars)

  cols <- sort(unique(names(mtcars)[names(mtcars) != "mpg"]))

  remove_shiny_inputs <- function(id, .input) {
    invisible(
      lapply(grep(id, names(.input), value = TRUE), function(i) {
        .subset2(.input, "impl")$.values$remove(i)
      })
    )
  }

  ui_lm <- function(id) {
    ns <- shiny::NS(id)
    uiOutput(ns("lmModel"))
  }

  server_lm <- function(id, parentSession) {
    moduleServer(
      id,
      function(input, output, session) {
        ns <- session$ns
        cli_alert_info(ns("lmModel"))
        output[["lmModel"]] <- renderUI({
          tags$div(
            id = environment(ns)[["namespace"]],
            div(
              environment(ns)[["namespace"]]
            )
          )
        })
      },
      session = parentSession
    )
  }

  ui_dynamic_module <- function(id = "dynamic_module") {
    ns <- NS(id)
    actionButton(ns("addButton"), "", icon = icon("plus"))
  }

  server_dynamic_module <- function(id = "dynamic_module",
                                    parentSession) {
    moduleServer(
      id,
      function(input, output, session) {
        observeEvent(input$addButton, {
          i <- sprintf("%04d", input$addButton)
          id <- sprintf("lmModel%s", i)
          insertUI(
            selector = "#dynamic_module-addButton",
            where = "beforeBegin",
            ui = ui_lm(id)
          )
          server_lm(id, parentSession)
          observeEvent(input[[paste0(id, "-deleteButton")]], {
            removeUI(selector = sprintf("#%s", id))
            remove_shiny_inputs(id, input)
          })
        })
      }
    )
  }

  ui <- function() {
    ui_dynamic_module()
  }



  server <- function(input, output, session) {
    server_dynamic_module(parentSession = session)
  }

  shinyApp(ui = ui, server = server)
}
