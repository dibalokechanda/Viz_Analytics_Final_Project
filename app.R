# Clear the work space
rm(list=ls())

#-------------------------------Load the Libraries------------------------------
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(tidyverse)
library(DT)
library(leaflet)
library(readxl)  

#-------------------------------Load the Data sets------------------------------
# Define the path
excel_path_listing <- "listings_data.xlsx"
excel_path_crime <- "crime_data_subset.xlsx"

# Load the data from excel
listing_data<- read_excel(excel_path_listing, sheet = 1) 
crime_data<- read_excel(excel_path_crime, sheet = 1) 

#Extract out certain cols from Data Frames


#-------------------------------Construct the Dashboard Sidebar-----------------
sidebar <- dashboardSidebar(
  width = 350,
  sidebarMenu(
    menuItem("Airbnb Listings and Crime Density", tabName = "listing"),
    menuItem("Crime Rate by Category", tabName = "crime"),
    menuItem("Listing Feedback", tabName = "feedback"),
    menuItem("About", tabName = "about")
  )
)
#-------------------------------Construct the Dashboard Body--------------------
body <- dashboardBody(
  chooseSliderSkin("Flat", color = "#F15B5F"),
  
  #-------------------------------Custom CSS -----------------------------------
  tags$head(tags$style(HTML(
          "table.dataTable tbody tr.selected td,
          table.dataTable tbody td.selected {
                border-top-color: white !important;
                box-shadow: inset 0 0 0 9999px #F15B5F !important;
            }
      
          table.dataTable tbody tr:active td {
                background-color: #F15B5F !important;
            }
      
          :root {
          --dt-row-selected: transparent !important;
            }
      
          table.dataTable tbody tr:hover, table.dataTable tbody tr:hover td {
            background-color: #F15B5F!important;
          }
          
          .custom-h3 {
          font-weight: bold;  # Extra bold
          color: #FF5733;  # Change color if desired
          }
        
         .leaflet-container {
          padding: 10px; 
          margin: 10px; 
          }
        
          "
  ))
  ),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  
#-------------------------------Tab 1 UI Code ----------------------------------
  tabItems(
    tabItem(tabName = "listing",
            h2("Listing Information and Crime Density")
    ),
    
#-------------------------------Tab 2 UI Code ----------------------------------
    
    tabItem(tabName = "crime",
            h2("Visualize Crime Rate by Category"),
    ),

#-------------------------------Tab 3 UI Code ----------------------------------
    tabItem(tabName = "feedback",
            h2("Provide Feedback for a Listing"),
            fluidRow(
              column(6,leafletOutput("map3",height = "700px")),
      # Feedback Form-----------------------------------------------------------
              column(6,
                     h4("ID: Select the listing where you stayed",class = "custom-h3"),
                     uiOutput("marker_info"),
                     sliderInput("slider1", "How safe did you feel during your stay?", min = 0, max = 10, value = 5),
                     sliderInput("slider2", "How safe did you feel about the neighborhood?", min = 0, max = 10, value = 5),
                     radioButtons("radio1", "Did anything happen with you during your stay?",
                                  choices = c("Yes" = "Yes", "No" = "No"),
                                  selected = "No"
                                               ),  
                     textAreaInput("textarea1", "Brief FeedBack", width = "500px"),
                     verbatimTextOutput("value"),
                     actionButton("submit", "Submit" ,class = "btn-danger",style="color:white"),
                   
                    
              ),
   
            ),
      
           fluidRow( # DT Table -----------------------------------------------------------------
                     column(6,
                            DTOutput("mytable")
                     )
                     )
    ),
#-------------------------------Tab 4 UI Code ----------------------------------

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
    #controlbar = dashboardControlbar(collapsed = FALSE, skinSelector()),

  ),
#-------------------------------Define the server logic ------------------------
  
  server <- function(input, output, session) {
#--------------- Tab  3 Leafletmap Logic ---------------------------------------
    output$map3 <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addCircleMarkers(
          data = listing_data,
          lat = ~Latitudec,
          lng = ~Longitudec,
          radius = 2,
          color = "#F15B5F",
          stroke = TRUE,
          fillOpacity = 0.5,
          popup = ~paste(
            "ID: <strong style='color:#F15B5F;'>", id, "</strong>",  
            "<br>", 
            "Room Type: <span style='color:black;'>", room_type, "</span>",
            "<br>",
            "Host Identify Verified: <span style='color:black;'>", host_identity_verified, "</span>",
            "<br>",
            "Price: <span style='color:black;'>", price, "</span>",
            "<br>",
            "<img src='", picture_url,"' width='300' height='300' />",
            "<br>"
            
          )
        )
    })
    
    observeEvent(input$map3_marker_click, {
      clicked_marker <- input$map3_marker_click
      
      # Find the corresponding row in the data frame
      clicked_info <- listing_data[
        listing_data$Latitudec == clicked_marker$lat &
          listing_data$Longitudec == clicked_marker$lng, ]
      
      # Display information about the clicked marker
      output$marker_info <- renderUI({
        HTML(
          paste(
            "<h4 style='color:#F15B5F; font-weight:bold'>", clicked_info$id, "</h3>"
          )
        )
      })
      
    })
    
#--------------- Reactive variable to store data for the Data Table-------------
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
      datatable(data(),colnames=c("Sequence", 
                                  "Safety of Listing",
                                  "Safey of Neighborhood",
                                  "Feedback", 
                                  "Incident Happend?"))
    })
  }
)