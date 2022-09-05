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
      expandOnHover = TRUE, collapsed = FALSE,
      shiny$div(
        if (getOption("development")) NULL else ui_devop()
      )
    ),
    body = bs4Dash$dashboardBody(
      bs4Dash$box(
        collapsable = TRUE, width = 12, title = "Filter",
        ui_metadata$ui()
      ),
      shiny$uiOutput("setupAnalysis")
    )
  )
}
