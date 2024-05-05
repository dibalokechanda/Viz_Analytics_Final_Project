# Fina Project Code for Visual Analytics | Spring 2024 

> This is the github repo which contains the code of the final project for Visual Analytics Course in Spring 2024. 



<img src="logo.png" width="100" height="100">

This is an app developed in RShiny named "SafeAirbnb". The app allows Airbnb guests to look up crime rates near an Airbnb listing and provides different filters like time range and category of crimes for more granular controls. Additionally, users can provide feedback related to the safety of the listing and the neighborhood around the listing where they stayed through a simple survey mechanism.


# Clone the Github Repo

Clone the repository with Github desktop or through a CLI with the following command:


```bash
git clone https://github.com/dibalokechanda/Viz_Analytics_Final_Project.git
```


# Packages Required


Install required packages with the following command

```R
if (!require(shiny)) install.packages("shiny")
if (!require(shinydashboard)) install.packages("shinydashboard")
if (!require(shinydashboardPlus)) install.packages("shinydashboardPlus")
if (!require(shinyWidgets)) install.packages("shinyWidgets")

if (!require(readxl)) install.packages("readxl")
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(DT)) install.packages("DT")
if (!require(sortable)) install
```