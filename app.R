# Load the Libraries
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)


sidebar <- dashboardSidebar(
  width = 350,
  sidebarMenu(
    menuItem("Airbnb Listings", tabName = "listing"),
    menuItem("Crime Rate", tabName = "crime"),
    menuItem("Feedback", tabName = "feedback"),
    menuItem("About", tabName = "about")
  )
)

body <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  tabItems(
    tabItem(tabName = "listing",
            h2("Listing Information and Crime Density")
    ),
    tabItem(tabName = "crime",
            h2("Visualize Crime Rate by Category")
    ),
    tabItem(tabName = "feedback",
            h2("Provide Feedback for a Listing")
    ),
    tabItem(tabName = "about",
            h2("About")
    )
  )
)

shinyApp(
  ui = dashboardPage(
    header =dashboardHeader(title = "Safe Airbnb"),
    sidebar=sidebar,
    body=body,
    skin = "red",
    controlbar = dashboardControlbar(collapsed = FALSE, skinSelector()),

  ),
  server = function(input, output) {
    
    
  }
)