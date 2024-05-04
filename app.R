# Clear the work space
rm(list=ls())

#-------------------------------Load the Libraries------------------------------
# Shiny Specific Libraries
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)

# Other Libraries
library(readxl)  
library(tidyverse)
library(DT)
library(sortable)
library(flexdashboard)
library(leaflet)


#-------------------------------Load the Data sets------------------------------
# Define the path
excel_path_listing <- "listings_data.xlsx"
excel_path_crime <- "crime_data_subset.xlsx"

# Load the data from excel
listing_data<- read_excel(excel_path_listing, sheet = 1) 
crime_data<- read_excel(excel_path_crime, sheet = 1)

# Get the unique elements in the "Primary Type" column
unique_elements <- unique(crime_data$`Primary Type`)

# Display the unique elements
print(unique_elements)

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
          
          .custom-h5 {
          font-weight: bold;  
          color: #FF5733;  
          }
          
          .custom-h3 {
          font-weight: bold;  
          color: black;
          padding: 0;
          margin: 0;
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
#-------------------------------Gauge Chart Code--------------------------------
            fluidRow(
            column(4, gaugeOutput("gauge1")),
            column(4, gaugeOutput("gauge2")),
            column(4, gaugeOutput("gauge3"))
            ),
            fluidRow(
              column(4,style = "text-align: center;",
                     h3("Violent Crime Index",class = "custom-h3")),
              column(4,style = "text-align: center;",
                     h3("Non-Violent Crime Index",class = "custom-h3")),
              column(4,style = "text-align: center;",
                     h3("Petty Crime Index",class = "custom-h3"))
            ),
#-----------------------------Drag and Sortable List----------------------------
      fluidRow(
        column(
          width = 12,
          bucket_list(
            header = "Drag and Drop",
            group_name = "bucket_list_group",
            orientation = "horizontal",
            add_rank_list(
              text = "Violent Crimes",
              labels = list(
               "HOMICIDE",
               "BATTERY",
               "ASSAULT",
               "SEX OFFENSE",
               "ROBBERY",
               "CRIMINAL SEXUAL ASSAULT",
               "INTIMIDATION",
               "KIDNAPPING",
               "STALKING"
              ),
              input_id = "rank_list_1"
            ),
            add_rank_list(
              text = "Non-Violent Crimes",
              labels = list(
                "THEFT",
                "MOTOR VEHICLE THEFT",
                "BURGLARY",
                "DECEPTIVE PRACTICE",
                "CRIMINAL DAMAGE",
                "NARCOTICS",
                "OFFENSE INVOLVING CHILDREN",
                "CRIMINAL TRESPASS",
                "WEAPONS VIOLATION",
                "ARSON",
                "INTERFERENCE WITH PUBLIC OFFICER",
                "LIQUOR LAW VIOLATION",
                "HUMAN TRAFFICKING"
              ),
              input_id = "rank_list_2"
            ),
            add_rank_list(
              text = "Petty Crimes",
              labels = list(
                "OTHER OFFENSE",
                "PUBLIC PEACE VIOLATION",
                "CONCEALED CARRY LICENSE VIOLATION",
                "OBSCENITY",
                "GAMBLING",
                "PUBLIC INDECENCY",
                "PROSTITUTION",
                "NON-CRIMINAL"
              ),
              input_id = "rank_list_3"
            ),
            add_rank_list(
              text = "Excluded Crimes",
              labels = NULL,
              input_id = "rank_list_4"
            )
          )
        )
      ),
    ),

#-------------------------------Tab 3 UI Code ----------------------------------
    tabItem(tabName = "feedback",
            h2("Provide Feedback for a Listing"),
            fluidRow(
              column(6,leafletOutput("map3",height = "700px")),
      # Feedback Form-----------------------------------------------------------
              column(6,
                     h5("ID: Select the listing where you stayed (Click on the Map)",class = "custom-h5"),
                     uiOutput("marker_info"),
                     sliderInput("slider1", "How safe did you feel during your stay?", min = 0, max = 10, value = 5),
                     sliderInput("slider2", "How safe did you feel about the neighborhood?", min = 0, max = 10, value = 5),
                     radioButtons("radio1", "Did anything happen with you during your stay?",
                                  choices = c("Yes" = "Yes", "No" = "No"),
                                  selected = "No"
                                               ),  
                     textAreaInput("textarea1", "Brief FeedBack", width = "500px"),
                     actionButton("submit", "Submit" ,class = "btn-danger",style="color:white"),
                     actionButton("clear_button", "Clear" ,class = "btn-danger",style="color:white"),
                   
                    
              ),
   
            ),
      # DT Table ---------------------------------------------------------------
           fluidRow(column(6,DTOutput("mytable")
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
    
    
#--------------- Tab 2 Gauge Chart Logic----------------------------------------
    
    output$gauge1 <- renderGauge({
      gauge(100, min=0, max=100,  sectors = gaugeSectors(success = c(0.5, 1), 
                                                        warning = c(0.3, 0.5),
                                                        danger = c(0, 0.3)))
    })
    
    output$gauge2 <- renderGauge({
      gauge(25, min=0, max=100,  sectors = gaugeSectors(success = c(0.5, 1), 
                                                        warning = c(0.3, 0.5),
                                                        danger = c(0, 0.3)))
    }) 
    
    output$gauge3 <- renderGauge({
      gauge(60, min=0, max=100,  sectors = gaugeSectors(success = c(0.5, 1), 
                                                        warning = c(0.3, 0.5),
                                                        danger = c(0, 0.3)))
    }) 
    
    
    listing_id_shared <- reactiveVal("")
    
#--------------- Tab  3 Clear Button Logic -------------------------------------
    observeEvent(input$clear_button, {
      # Reset the slider inputs
      updateSliderInput(session, "slider1", value = 5)
      updateSliderInput(session, "slider2", value = 5)
      
      # Reset the radio buttons
      updateRadioButtons(session, "radio1", selected = "No")
      
      # Clear the text area
      updateTextAreaInput(session, "textarea1", value = "")
      
      # Reset the marker_info output
      output$marker_info <- renderText({
        ""
      })
    })
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
      
      listing_id_shared(clicked_info$id)
      # Display information about the clicked marker
      output$marker_info <- renderUI({
        HTML(
          paste(
            "<h5 style='color:#F15B5F; font-weight:bold'>", clicked_info$id, "</h5>"
          )
        )
      })
      
    })
    
#--------------- Reactive variable to store data for the Data Table-------------
    data <- reactiveVal(data.frame(ListingId=character(0),
                                   Slider1 = numeric(0), 
                                   Slider2 = numeric(0),
                                   Textarea1 = character(0), 
                                   Radio = character(0)))
    
    # Observe the submit button click event
    observeEvent(input$submit, {
      # Get the inputs from the UI
      listing_id_s<-listing_id_shared()
      
      print(listing_id_s)
      slider1 <- input$slider1
      slider2 <- input$slider2
      textarea1 <- input$textarea1
      radio1 <- input$radio1
      
      # Create a new row of data
      new_row <- data.frame(ListingId= listing_id_s,
                            Slider1 = slider1, 
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
                                  "Listing ID",
                                  "Safety of Listing",
                                  "Safey of Neighborhood",
                                  "Feedback", 
                                  "Incident Happend?"))
    })
  }
)