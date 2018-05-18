library(shiny)
library(shinydashboard)
library(DT)
library(futile.logger)
futile.logger::flog.threshold(DEBUG)

flog.debug("Running example...")

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      # Setting id makes input$tabs give the tabName of currently-selected tab
      id = "tabs",
      menuItem(
        "Dashboard",
        tabName = "dashboard",
        icon = icon("dashboard")
      )
    )),
  dashboardBody(
    shinydashboard::box(width = 12, tabsetPanel(
      tabPanel(
        title = "Config yaml",
        DTmoduleUI("DTmodule")
      )
    ))
  )
)


server <- function(input, output) {
  callModule(DTmodule, "DTmodule",
             data = mtcars,
             checkbox.colname = "Select",
             actions.colname = "Actions",
             checked.rows.id.prefix = "Row",
             header = "Enchanced DataTable",
             add.row = TRUE,
             delete.row = TRUE,
             is.checked.rule = quote(gear == 4))
}

shinyApp(ui, server)