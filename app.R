# Load the Libraries
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(DT)


# Construct the Dashboard Sidebar
sidebar <- dashboardSidebar(
  width = 350,
  sidebarMenu(
    menuItem("Airbnb Listings and Crime Density", tabName = "listing"),
    menuItem("Crime Rate by Category", tabName = "crime"),
    menuItem("Listing Feedback", tabName = "feedback"),
    menuItem("About", tabName = "about")
  )
)

# Construct DashBoard Body
body <- dashboardBody(
  chooseSliderSkin("Flat", color = "#F15B5F"),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  tabItems(
    tabItem(tabName = "listing",
            h2("Listing Information and Crime Density")
    ),
    tabItem(tabName = "crime",
            h2("Visualize Crime Rate by Category"),
    ),
    tabItem(tabName = "feedback",
            h2("Provide Feedback for a Listing"),
            fluidRow(
              column(12,
                     textInput("text1", "Listing ID"),
                     sliderInput("slider1", "How safe did you feel during your stay?", min = 0, max = 10, value = 5),
                     sliderInput("slider2", "How safe did you feel about the neighborhood?", min = 0, max = 10, value = 5),
                     radioButtons("radio1", "Did anything happen with you during your stay?",
                                  choices = c("Yes" = "Yes", "No" = "No"),
                                  selected = "No"
                                               ),  
                     textAreaInput("caption", "Brief Feedback", width = "500px"),
                     verbatimTextOutput("value"),
                     actionButton("submit", "Submit" ,class = "btn-danger",style="color:white")
              )
            ),
            
            
    ),
    tabItem(tabName = "about",
            h2("About")
    )
  )
)


# Construct the dashboard app

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