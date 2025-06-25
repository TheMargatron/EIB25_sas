# =============================================================================
# Title:        EIB25 Surfers Against Sewage Shiny dashboard
# Description:  Shiny dashboard for 
# Author:       Margaret Bolton <mb804 (at) exeter.ac.uk>
# Created:      2025-06-23
# Last updated: 2025-06-23
# Dependencies: shiny, leaflet, dplyr, sf
# Inputs:       model outputs from /data/predictions.rds
# Outputs:      Shiny app interface (hosted online)
# =============================================================================

#### Libraries ####
library(tidyverse)
library(here)
library(shiny)
library(sf)
library(tmap)
library(leaflet)
library(rnaturalearth)
library(xlsx)

#### setup ####
CSO_data <- 
  xlsx::read.xlsx(
    here::here("Data", "CSO Database.xlsx"), 
    sheetIndex = 1
  ) 

sewage_events_data <- 
  read.csv(
    here::here("Data", "Sewage events 2025.csv")
  ) 

#### First visualisation ####

## prep the data
sewage_processed_data <- sewage_events_data %>% 
  filter(!if_all(everything(), ~ is.na(.) | . == "")) %>% 
  filter(!is.na(Longitude), !(Longitude == 0 & Latitude == 0)) %>% 
  # mutate(across(c(Asset.ID, Event.Type), as.factor)) %>% 
  rename(lng = Longitude, lat = Latitude) %>% 
  mutate(across(c(Start.Date, End.Date), ~as.Date(.x, format = "%d/%m/%Y"))) 

SWW_data <- sewage_processed_data %>% 
  filter(Asset.ID == "South West")

SWW_tags <- SWW_data %>% 
  select(lng, lat, Water.Company, Asset.ID) %>% 
  distinct()

# discarded idea to have date slider displaying status by day because it's data intensive
# easier, cheaper, more readable to display a hist or some other data summary
# SWW_test_data <- expand.grid(Event.Date = seq(min(SWW_data$Start.Date, SWW_data$End.Date, na.rm = TRUE), 
#                                               max(SWW_data$Start.Date, SWW_data$End.Date, na.rm = TRUE),
#                                               by = "1 day"),
#                              Asset.ID = "South West",
#                              Water.Company = unique(SWW_data$Water.Company))

  
# pal <- colorFactor(palette = "Set1", domain = SWW_data$Water.Company)

## make the map 
# Chose Positron for now, but ideally would use one which has blue sea
# E.g. MapTiler Basic, but that requires API and limited usage

ui <- fluidPage(
  # titlePanel("SAS data vis map"),
  fluidRow(
    column(width = 3, plotOutput("barplot", height = "300px")),
    column(width = 7, leafletOutput("sasmap", height = "600px"))
    )
)

server <- function(input, output, session) {
  
  # make the map
  output$sasmap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -4.3, lat = 50.55, zoom = 8) %>%
      addMarkers(lng = -3.5, lat = 50.7, popup = "Exeter") %>% 
      addCircleMarkers(data = SWW_tags, ~lng, ~lat, 
                       radius = 3, weight = 1,
                       fillColor = "#06BCC1", color = "black", fill = TRUE,
                       layerId = ~Water.Company, popup = ~Water.Company)
  })
  
  # start off unclicked
  clicked_site <- reactiveVal(NULL)
  
  # change to clicked if clicked
  observeEvent(input$sasmap_marker_click, {
    clicked_site(input$sasmap_marker_click$id)
  })
  
  # render a histogram
  output$barplot <- renderPlot({
    req(clicked_site())  # Only run if a marker was clicked
    bar_dat <- SWW_data %>% filter(Water.Company == clicked_site())
    print(bar_dat)
    
    ggplot(bar_dat, aes(x = Event.Type)) +
      geom_bar() 
  })
  
}

shinyApp(ui, server)

