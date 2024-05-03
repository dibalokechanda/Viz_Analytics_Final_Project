library(shiny)
library(shinydashboard)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Crime Rate", tabName = "crime"),
    menuItem("Feedback", tabName = "feedback")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "crime",
            h2("Visualize Crime Rate by Category")
    ),
    
    tabItem(tabName = "feedback",
            h2("Provide Feedback for a Listing")
    )
  )
)

shinyApp(
  ui = dashboardPage(
    dashboardHeader(title = "Safe Airbnb"),
    sidebar,
    body,
  ),
  server = function(input, output) {
    
    
  }
)