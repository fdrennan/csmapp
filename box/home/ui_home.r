#' @export
ui <- function() {
  box::use(shiny)
  box::use(bs4Dash)
  box::use(.. / metadata / ui_metadata)
  box::use(.. / devop / ui_devop)


  bs4Dash$dashboardPage(
    controlbar = bs4Dash$dashboardControlbar(
      id = "homeControlbar", collapsed = FALSE,
      shiny$div(
        class = "p-3",
        if (getOption("development")) NULL else ui_devop(),
        ui_metadata$ui()
      )
    ),
    dark = TRUE,
    header = bs4Dash$dashboardHeader("CSM Management System"),
    sidebar = bs4Dash$dashboardSidebar(
      disable = FALSE,
      expandOnHover = FALSE,
      collapsed = FALSE,
      bs4Dash$sidebarMenu(
        id = "sidebarMenu",
        bs4Dash$menuItem(
          text = "Tab 1",
          tabName = "tab1"
        ),
        bs4Dash$menuItem(
          text = "Tab 2",
          tabName = "tab2"
        )
      )
    ),
    body = bs4Dash$dashboardBody(
      shiny$uiOutput("setupAnalysis")
    )
  )
}
