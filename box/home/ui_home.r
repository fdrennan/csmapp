#' @export
ui <- function() {
  box::use(shiny)
  box::use(bs4Dash)
  box::use(.. / metadata / ui_metadata)
  box::use(.. / devop / ui_devop)


  bs4Dash$dashboardPage(
    dark = TRUE,
    header = bs4Dash$dashboardHeader("CSM Management System"),
    sidebar = bs4Dash$dashboardSidebar(
      expandOnHover = TRUE, collapsed = TRUE,
      shiny$div(
        if (getOption("development")) NULL else ui_devop(),
        ui_metadata$ui()
      )
    ),
    body = bs4Dash$dashboardBody(
      shiny$uiOutput("setupAnalysis")
    )
  )
}
