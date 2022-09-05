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
  cli_alert_info('ui_lm id is {id}')
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("lmModel"))
}

server_lm <- function(id) {
  cli_alert_info('server_lm id is {id}')
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      output$lmModel <- renderUI({
        tags$div(
          id = environment(ns)[["namespace"]],
          h1("Hello")
        )
      })
    }
  )
}

ui_dynamic_module <- function(id='dynamic_module') {
  ns <- NS(id)
  cli_alert_info('ui_dynamic_module id is {id}')
  fluidPage(
    actionButton(ns("addButton"), "", icon = icon("plus"))
  )
}

server_dynamic_module <- function(id='dynamic_module') {
  cli_alert_info('server_dynamic_module id is {id}')
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
        server_lm(id)
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



server <- function(input, output) {
  server_dynamic_module()
}

shinyApp(ui = ui, server = server)

 