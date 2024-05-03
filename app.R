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
              column(6,
                     textInput("text1", "Listing ID"),
                     sliderInput("slider1", "How safe did you feel during your stay?", min = 0, max = 10, value = 5),
                     sliderInput("slider2", "How safe did you feel about the neighborhood?", min = 0, max = 10, value = 5),
                     radioButtons("radio1", "Did anything happen with you during your stay?",
                                  choices = c("Yes" = "Yes", "No" = "No"),
                                  selected = "No"
                                               ),  
                     textAreaInput("textarea1", "Brief FeedBack", width = "500px"),
                     verbatimTextOutput("value"),
                     actionButton("submit", "Submit" ,class = "btn-danger",style="color:white")
              ),
              column(6,
                     DTOutput("mytable")
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
  # Define the server logic
  server <- function(input, output, session) {
    # Reactive variable to store data for the DataTable
    data <- reactiveVal(data.frame(Slider1 = numeric(0), 
                                   Slider2 = numeric(0),
                                   Textarea1 = character(0), 
                                   Radio = character(0)))
    
    # Observe the submit button click event
    observeEvent(input$submit, {
      # Get the inputs from the UI
      slider1 <- input$slider1
      slider2 <- input$slider2
      textarea1 <- input$textarea1
      radio1 <- input$radio1
      
      # Create a new row of data
      new_row <- data.frame(Slider1 = slider1, 
                            Slider2 = slider2,
                            Textarea1 = textarea1,
                            Radio = radio1)
      
      # Add the new row to the existing data
      current_data <- data()  # Get current data
      updated_data <- rbind(current_data, new_row)  # Add new row to the data
      
      # Update the reactive variable with the new data
      data(updated_data)
    })
    
    # Render the DataTable with the stored data
    output$mytable <- renderDT({
      datatable(data())
    })
  }
)