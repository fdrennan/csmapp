#' @export
ui <- function() {
  box::use(shiny)
  box::use(bs4Dash)
  box::use(.. / metadata / ui_metadata)
  box::use(.. / devop / ui_devop)


  bs4Dash$dashboardPage(controlbar = bs4Dash$dashboardControlbar(id='homeControlbar', collapsed = FALSE,
    shiny$div(
      if (getOption("development")) NULL else ui_devop(),
      ui_metadata$ui()
    )
  ),
    dark = TRUE,
    header = bs4Dash$dashboardHeader("CSM Management System"),
    sidebar = bs4Dash$dashboardSidebar(
      expandOnHover = FALSE, collapsed = TRUE
    ),
    body = bs4Dash$dashboardBody(
      shiny$uiOutput("setupAnalysis")
    )
  )
}
