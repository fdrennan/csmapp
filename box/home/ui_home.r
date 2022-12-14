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
        # flat = FALSE,
        id = "sidebarMenu",
        bs4Dash$menuItem(icon =shiny::icon("book"),
          text = "Welcome",
          tabName = "tab0"
        ),
        bs4Dash$menuItem(
          text = "Flagging Setup",
          tabName = "tab2", icon = shiny$icon("flag")
        ),
        bs4Dash$menuItem(
          text = "Flagging Review",
          tabName = "tab3", icon = shiny$icon("check")
        ),
        bs4Dash$menuItem(
          text = "Configuration Parameters",
          tabName = "tab4", icon = shiny$icon("gears")
        )
      )
    ),
    body = bs4Dash$dashboardBody(
      bs4Dash$tabItems(
        bs4Dash$tabItem(
          tabName = "tab0", shiny$h2("Centralized Statistical Monitoring", class = "text-display"),
          shiny$includeHTML('./README.html')
        ),
        bs4Dash$tabItem(
          tabName = "tab4",
          bs4Dash$box(
            title = shiny$h2("Configuration Parameters"),
            width = 8, offset = 2,
            shiny$h4("Compare Proportion"),
            shiny$numericInput("T_Zscore", "T_Zscore", min = -Inf, max = Inf, value = 1.68),
            shiny$numericInput("min_n_number_betabinom", "min_n_number_betabinom", min = -Inf, max = Inf, value = 5),
            shiny$numericInput("min_n_value", "min_n_value", min = -Inf, max = Inf, value = 2),
            shiny$h4("Tukey"),
            shiny$selectInput("TukeyOutliers", "TukeyOutliers", choices = c("inner", "output"), selected = "outer"),
            shiny$h4("Dosing Analysis"),
            shiny$numericInput("cutoff_perplanned", "cutoff_perplanned", min = -Inf, max = Inf, value = 80)
          )
        ),
        bs4Dash$tabItem(
          tabName = "tab2",
          shiny$uiOutput("tab2UI")
        ),
        bs4Dash$tabItem(
          tabName = "tab3",
          shiny$actionButton("updateReview", "Review"),
          shiny$textOutput("reviewOut", container = shiny$tags$pre)
        )
      )
    )
  )
}
