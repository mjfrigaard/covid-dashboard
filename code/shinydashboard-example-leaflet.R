# =====================================================================#
# This is code to create:  Create a value box for the maximum velocity
# Version: 4.9
# =====================================================================#

## 4.9 - Update the look of your new application

# Now let's put it all together. You have created an application for viewing
# fireball locations. The full application is shown in the sample code. Update
# the skin of the dashboard page to a color of your choosing.

## INSTRUCTIONS

# Update the skin of the dashboard page.
# Rerun the application to view this change.



# packages --------------------------------------------------------------

library(leaflet)
library(shiny)
library(shinydashboard)
library(tidyverse)



# data inputs -----------------------------------------------------------

nasa_fireball <- read_rds("../data/nasa_fireball.rds")
max_impact_e <- max(nasa_fireball$impact_e)
max_energy <- max(nasa_fireball$energy)
max_vel <- max(nasa_fireball$vel, na.rm = TRUE)


# define the body -------------------------------------------------------

body <- dashboardBody(
 fluidRow(
   
    valueBox(
      value = max_energy,
      subtitle = "Maximum total radiated energy (Joules)",
      icon = icon("lightbulb-o")
      
    ),
    valueBox(
      
      value = max_impact_e,
      
      subtitle = "Maximum impact energy (kilotons of TNT)",
      
      icon = icon("star")
      
    ),
    valueBox(
      
      value = max_vel,
      
      subtitle = "Maximum pre-impact velocity",
      
      icon = icon("fire")
      
    )
  ),
 
  fluidRow(
    
    leafletOutput("plot")
    
  )
 
)

# define the UI ---------------------------------------------------------


ui <- dashboardPage(
  
  skin = "purple",
  
  header = dashboardHeader(),
  
  sidebar = dashboardSidebar(),
  
  body = body
  
)


# define the server -----------------------------------------------------

server <- function(input, output) {
  
  output$plot <- renderLeaflet({
    
    leaflet() %>%
      
      addTiles() %>%
      
      addCircleMarkers(
        
        lng = nasa_fireball$lon,
        
        lat = nasa_fireball$lat,
        
        radius = log(nasa_fireball$impact_e),
        
        label = nasa_fireball$date,
        
        weight = 2)
    
    })
}


shinyApp(ui, server)
