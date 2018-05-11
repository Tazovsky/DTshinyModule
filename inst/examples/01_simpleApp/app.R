library(shiny)
library(shinydashboard)
library(DT)

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
             checked.rows.id.prefix = "Row")
}

shinyApp(ui, server)