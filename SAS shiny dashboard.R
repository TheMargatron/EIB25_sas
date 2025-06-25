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



#### Prep the data ####
sewage_processed_data <- sewage_events_data %>% 
  filter(!if_all(everything(), ~ is.na(.) | . == "")) %>% 
  filter(!is.na(Longitude), !(Longitude == 0 & Latitude == 0)) %>% 
  # mutate(across(c(Asset.ID, Event.Type), as.factor)) %>% 
  rename(lng = Longitude, lat = Latitude) %>% 
  mutate(Start.DT = as.POSIXct(paste(Start.Date, Start.Time, sep = " "), format = "%d/%m/%Y %H:%M"),
         End.DT = as.POSIXct(paste(End.Date, End.Time, sep = " "), format = "%d/%m/%Y %H:%M"),
         across(c(Start.Date, End.Date), ~as.Date(.x, format = "%d/%m/%Y"))) 

SWW_data <- sewage_processed_data %>% 
  filter(Asset.ID == "South West") %>% 
  filter(!is.na(End.DT), !is.na(Start.DT)) 

SWW_tags <- SWW_data %>% 
  select(lng, lat, Water.Company, Asset.ID) %>% 
  distinct()

SWW_min_dt <- min(SWW_data$Start.DT, na.rm = TRUE)
SWW_max_dt <- max(SWW_data$End.DT, na.rm = TRUE)

SWW_base <- expand.grid(Start.DT = SWW_min_dt,
                        End.DT = SWW_max_dt,
                        Water.Company = unique(SWW_data$Water.Company)) 

generate_full_status <- function(df_site) {
  # Ensure proper ordering
  df_site <- df_site %>% arrange(Start.DT)
  
  # Create "off" intervals in gaps between end[i] and start[i+1]
  off_intervals <- df_site %>%
    mutate(next_start = lead(Start.DT)) %>%
    filter(!is.na(next_start) & End.DT < next_start) %>%
    transmute(
      # Water.Company = first(Water.Company),
      Start.DT = End.DT,
      End.DT = next_start,
      Event.Type = "none"
    )
  
  # Combine original and "off" intervals
  bind_rows(df_site, off_intervals) %>%
    arrange(Start.DT)
}



# SWW_processed <- SWW_data %>% 
#   filter(!is.na(End.DT), !is.na(Start.DT)) %>% 
#   mutate(Duration.DT = End.DT - Start.DT) 

# SWW_off <- SWW_data %>%
#   group_by(Water.Company) %>% 
#   arrange(Start.DT) %>%
#   mutate(next_start = lead(Start.DT)) %>%
#   filter(!is.na(next_start)) %>%
#   filter(End.DT < next_start) %>%
#   transmute(
#     Start.DT = End.DT,
#     End.DT = next_start,
#     Event.Type = "none"
#   )

SWW_processed <- SWW_data %>%
  group_by(Water.Company) %>%
  group_modify(~ generate_full_status(.x)) %>%
  ungroup()

# Combine all intervals
all_intervals <- bind_rows(intervals, off_intervals) %>%
  arrange(start)

SWW_processed %>%
  mutate(Status = factor(Event.Type, 
                         levels = c("spill", "maintenance", "none"),
                         labels = c("Spill", "Maintenance", "None"),
                         ordered = TRUE)) %>% 
  filter(Water.Company == "SWW0853") %>%
  ggplot(aes(y = 0, col = Status)) +
  geom_segment(aes(x = Start.DT, xend = End.DT), size = 6, alpha = 0.7) +
  ylim(c(-1,1)) +
  labs(x = "Time", y = "Category") +
  theme_void() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "top",
        aspect.ratio = 0.3) +
  scale_colour_manual(
    values = c("#cf121d", "#585c62", "#1d9b7e"),
    drop = FALSE
  )

  

# discarded idea to have date slider displaying status by day because it's data intensive
# easier, cheaper, more readable to display a hist or some other data summary
# SWW_test_data <- expand.grid(Event.Date = seq(min(SWW_data$Start.Date, SWW_data$End.Date, na.rm = TRUE), 
#                                               max(SWW_data$Start.Date, SWW_data$End.Date, na.rm = TRUE),
#                                               by = "1 day"),
#                              Asset.ID = "South West",
#                              Water.Company = unique(SWW_data$Water.Company))

  
# pal <- colorFactor(palette = "Set1", domain = SWW_data$Water.Company)

#### make the map ####
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
    
    SWW_spills <- SWW_processed %>% 
      filter(Water.Company == clicked_site()) %>%
      filter(Event.Type == "spill")
    
    SWW_filtered <- SWW_processed %>% 
      filter(Water.Company == clicked_site()) %>%
      mutate(Status = factor(Event.Type, 
                             levels = c("spill", "maintenance", "none"),
                             labels = c("Spill", "Offline", "None"),
                             ordered = TRUE))
    ggplot() +
      geom_point(data = SWW_spills, aes(y = 0.5, x = Start.DT), size = 3, shape = 6, color = "#cf121d") +
      geom_segment(data = SWW_filtered, aes(y = 0, x = Start.DT, xend = End.DT, color = Status), size = 6, show.legend = TRUE) +
      ylim(c(-1,1)) +
      labs(x = "Time", y = "Category") +
      theme_void() +
      theme(axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            legend.position = "top",
            aspect.ratio = 0.3) +
      scale_colour_manual(
        values = c("#cf121d", "#585c62", "#1d9b7e"),
        drop = FALSE
      )

  })
  
}


shinyApp(ui, server)


