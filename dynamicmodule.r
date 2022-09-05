# library(shiny)
data(mtcars)
cols <- sort(unique(names(mtcars)[names(mtcars) != 'mpg']))
 

lmUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("lmModel"))
}

lmModelModule <- function(input, output, session) {
  lmModel <- reactive({
    lm(sprintf('mpg ~ %s',paste(input$vars, collapse = '+')), data = mtcars)
  })
  output[['lmModel']] <- renderUI({
    ns <- session$ns
    tags$div(id = environment(ns)[['namespace']],
             tagList(
               wellPanel(
                 fluidRow(
                   column(3,
                          tags$h3('Build a Linear Model for MPG'),
                          selectInput(ns('vars'),
                                      'Select dependent variables',
                                      choices = cols,
                                      selected = cols[1:2],
                                      multiple = TRUE)),
                   column(4, 
                          renderPrint({summary(lmModel())})
                   ),
                   column(4, 
                          renderPlot({par(mfrow = c(2,2))
                            plot(lmModel())})
                   ),
                   column(1,
                          actionButton(ns('deleteButton'),
                                       '',
                                       icon = shiny::icon('times'),
                                       style = 'float: right')
                   )
                 )
               )
             )
    )
  })
}


ui <- fluidPage(
  br(),
  actionButton('addButton', '', icon = icon('plus'))
)
server <- function(input, output) {
  observeEvent(input$addButton, {
    i <- sprintf('%04d', input$addButton)
    id <- sprintf('lmModel%s', i)
    insertUI(
      selector = '#addButton',
      where = "beforeBegin",
      ui = lmUI(id)
    )
    callModule(lmModelModule, id)
    observeEvent(input[[paste0(id, '-deleteButton')]], {
      removeUI(selector = sprintf('#%s', id))
      remove_shiny_inputs(id, input)
    })
  })
}
shinyApp(ui = ui, server = server)



shinyApp(ui = ui, server = server)