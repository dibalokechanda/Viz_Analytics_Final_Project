# Author : Dibaloke Chanda
# Course: Visual Analytics | Spring 2024
# Final Project 

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
library(ggplot2)
library(DT)
library(sortable)
library(flexdashboard)
library(leaflet)
library(ggthemes)

#-------------------------------Load the Data sets------------------------------
# Define the path
excel_path_listing <- "listings_data.xlsx"
excel_path_crime <- "crime_data_subset.xlsx"

# Load the data from excel
listing_data<- read_excel(excel_path_listing, sheet = 1) 
crime_data<- read_excel(excel_path_crime, sheet = 1)

#---------------------------Perform Pre-processing------------------------------

# Mutate the time stamp columns
crime_data <- crime_data %>%
  mutate(Date = ymd_hms(Date))  

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
            color: black;  
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
         
         .rank-list-title{
            color:red;
            font-weight: bold;
            font-size:18px
         }
      
        .rank-list-item{
            font-size:14px
        }
        
        .default-sortable .rank-list-item.sortable-chosen, .default-sortable .rank-list-item.sortable-ghost.sortable-chosen, 
        .default-sortable .rank-list-item.sortable-drag{
            background-color: red;
            color:white;
            border: 1px solid red;
        }
        
      .default-sortable .rank-list-item:hover:not(.disabled) {
          background-color: #FF5733;
          color:white;
          cursor: grab;
      }
      .default-sortable .rank-list-item.sortable-ghost {
          color: transparent;
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
            HTML("<br><br><br><br>"),
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

#-------------------------------Tab 2 Date Slider ------------------------------
fluidRow(
  column(4),
  column(8,uiOutput('ui_big')
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

#-----------------------------Tab 2 Bar Chart Code------------------------------
fluidRow(
  column(2),column(8,plotOutput("crimeBarChart",height = "600px"),column(2))),
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
           fluidRow(column(6,DTOutput("mytable"),column(6, textOutput("selected_listing_id")))
               ),
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

#----------------------- Tab 2 Date Slider--------------------------------------
    output$ui_big <- renderUI({
      tagList(
        tags$style(type = 'text/css', '#big_slider .irs-grid-text {font-size: 20px}'), 
        div(id = 'big_slider',
            sliderInput(
              width = 800,
              "dateRange",  
              label = "",  
              min = min(crime_data$Date),  # Minimum date
              max = max(crime_data$Date),  # Maximum date
              value = c(min(crime_data$Date), max(crime_data$Date)),  # Default range
              timeFormat = "%Y-%m-%d"   # Date format
            ),  
        )
      )
    })
    
#----------------------- Reactive Value of Crime Count -------------------------
crime_counts_date_subsetted_data<- reactiveVal("")    

#----------------------- Tab 2 Date Slider Subsetting Logic---------------------
    observeEvent(input$dateRange, {
      # Get the selected date range
      subset_date_range <- input$dateRange

      # Subset the data frame to the selected date range
      crime_data_filtered <- crime_data %>%
        filter(Date >= subset_date_range[1] & Date <= subset_date_range[2])
      
      
      # Count the occurrences of each crime type
      crime_type_counts <- crime_data_filtered %>%
        count(`Primary Type`)
      
      crime_counts_date_subsetted_data(crime_type_counts)
      })

#-------------Tab 2 Drag Drop Logic---------------------------------------------
violent_crimes_list<-reactive({input$rank_list_1}) 
non_violent_crimes_list <-reactive({input$rank_list_2}) 
petty_crimes_list <-reactive({input$rank_list_3})


#------------Top 5 Bar Chart Logic----------------------------------------------
top_5_crime_counts <- reactive({
  crime_counts_date_subsetted_data() %>%
    arrange(desc(n)) %>%
    slice_head(n =12)  # Select the top 5 based on "n"
})

output$crimeBarChart <- renderPlot({
  ggplot(top_5_crime_counts(), aes(x = reorder(`Primary Type`, n), y = n)) +
    geom_bar(stat = "identity", fill = "#FF5733") + 
    geom_text(aes(label = n), hjust=1.5, color = "white", size = 5) +
    coord_flip() +
    labs(title = "Top 12 Crime Counts by Crime Type") +  # Adding the plot title
    theme_wsj(color = "#ECF0F5",)+  
    theme(
      plot.title = element_text(
        size = 30,
        face = "bold",
        hjust = 0.5,  # Center-align the title
        margin = margin(b = 20)  # Add bottom margin (padding)
      ),
      axis.text.y = element_text(size = 14),
      panel.background = element_rect(fill = "#ECF0F5"),  # Panel background
      plot.background = element_rect(fill = "#ECF0F5")  # Overall plot background
    )
})


#--------------- Tab 2 Gauge Chart Value Logic----------------------------------

crime_percentages <- reactive({
  # Total crime count
  total_crime_count <- sum(crime_counts_date_subsetted_data()$n)
  
  # Compute violent crime percentage
  violent_crime_percentage <- crime_counts_date_subsetted_data() %>%
    filter(`Primary Type` %in% violent_crimes_list()) %>%
    summarize(percentage = (sum(n) / total_crime_count) * 100) %>%
    .$percentage
  
  # Compute non-violent crime percentage
  non_violent_crime_percentage <- crime_counts_date_subsetted_data() %>%
    filter(`Primary Type` %in% non_violent_crimes_list()) %>%
    summarize(percentage = (sum(n) / total_crime_count) * 100) %>%
    .$percentage
  
  # Compute petty crime percentage
  petty_crime_percentage <- crime_counts_date_subsetted_data() %>%
    filter(`Primary Type` %in% petty_crimes_list()) %>%
    summarize(percentage = (sum(n) / total_crime_count) * 100) %>%
    .$percentage
  
  # Return a list with all three percentages
  list(
    violent = violent_crime_percentage,
    non_violent = non_violent_crime_percentage,
    petty = petty_crime_percentage
  )
})

# Render the filtered crime counts as a table
output$crimeScores <- renderTable({
  crime_percentages()$violent
})

#--------------- Tab 2 Gauge Chart Render Logic---------------------------------
    
    output$gauge1 <- renderGauge({
      gauge(crime_percentages()$violent, min=0, max=100,  
            sectors = gaugeSectors(success = c(0.5, 1), 
                                   warning = c(0.3, 0.5),
                                   danger = c(0, 0.3)))
    })
    
    output$gauge2 <- renderGauge({
      gauge(crime_percentages()$non_violent, min=0, max=100, 
            sectors = gaugeSectors(success = c(0.5, 1), 
                                   warning = c(0.3, 0.5),
                                   danger = c(0, 0.3)))
    }) 
    
    output$gauge3 <- renderGauge({
      gauge(crime_percentages()$petty, min=0, max=100, 
            sectors = gaugeSectors(success = c(0.5, 1), 
                                   warning = c(0.3, 0.5),
                                   danger = c(0, 0.3)))
    }) 
    
#-----------------Reactive Value to be used in Tab 3----------------------------

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
    
    # Rendering Map------------------------------------------------------------
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
    
    # Map Click Observer--------------------------------------------------------
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
#--------------- Tab 3 Submit Button Click Event to Update DT Table-------------
    
     # Observe the submit button click event
    observeEvent(input$submit, {
      # Get the inputs from the UI
      listing_id_s<-listing_id_shared()
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
    
#----------------- Render the DataTable with the stored data--------------------
    output$mytable <- renderDT({
      datatable(data(),selection = "single",colnames=c("Sequence", 
                                  "Listing ID",
                                  "Safety of Listing",
                                  "Safey of Neighborhood",
                                  "Feedback", 
                                  "Incident Happend?"))
    })
    
#-----------------Check Individual Feedback-------------------------------------
    
    # Observe click events on the DT table and update the leaflet map
    observeEvent(input$mytable_rows_selected, {
      selected_row <- input$mytable_rows_selected  
      print(selected_row )
      if (length(selected_row) > 0) {
        # Extract the corresponding data
        selected_listing <- listing_data[selected_row, ]
        # Render a new leaflet map with only the selected listing
        output$map3 <- renderLeaflet({
          leaflet() %>%
            addTiles() %>%
            addCircleMarkers(
              lat = selected_listing$Latitudec,
              lng = selected_listing$Longitudec,
              radius = 6,
              color = "#FF0000",
              stroke = TRUE,
              fillOpacity = 0.7,
              popup = paste(
                "ID: <strong style='color:#F15B5F;'>", selected_listing$id, "</strong>",
                "<br>",
                "Room Type: <span style='color:black;'>", selected_listing$room_type, "</span>",
                "<br>",
                "Host Identity Verified: <span style='color:black;'>", selected_listing$host_identity_verified, "</span>",
                "<br>",
                "Price: <span style='color:black;'>", selected_listing$price, "</span>",
                "<br>",
                "<img src='", selected_listing$picture_url, "' width='300' height='300' />"
              )
            )
        })
      }
    })
    
    
  }
)