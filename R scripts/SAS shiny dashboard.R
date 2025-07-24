# =============================================================================
# Title:        EIB25 Surfers Against Sewage Shiny dashboard
# Description:  Shiny dashboard for EIB Grand Challenge
# Author:       Margaret Bolton <mb804 (at) exeter.ac.uk>
# Created:      2025-06-23
# Last updated: 2025-06-27
# Dependencies: shiny, leaflet, dplyr, sf
# Inputs:       aggregated static data from water company APIs
# Outputs:      Shiny app interface
# =============================================================================

#### Libraries ####
library(tidyverse)
library(here)
library(shiny)
library(sf)
library(leaflet)
library(tmap)
library(rnaturalearth)
library(xlsx)
# library(rsconnect)

#### setup ####
# CSO_data <-
#   xlsx::read.xlsx(
#     here::here(dirname(here::here()), "Data", "CSO Database.xlsx"),
#     sheetIndex = 1
#   )

sewage_events_data <- 
  read.csv(
    here::here(dirname(here::here()), "Data", "Sewage events 2025.csv")
  ) 

constituencies <- sf::read_sf(dsn = here::here(dirname(here::here()), "Data", "Constituencies_July_2024")) %>% 
  st_transform(shape, crs = 4326)

# tm_shape(constituencies) + tm_polygons()

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

SWW_data_sf <- sf::st_as_sf(SWW_data, crs = 4326, coords = c("lng", "lat"))

SWW_tags <- SWW_data %>% 
  select(lng, lat, Water.Company, Asset.ID) %>% 
  distinct()

SWW_min_dt <- min(SWW_data$Start.DT, na.rm = TRUE)
SWW_max_dt <- max(SWW_data$End.DT, na.rm = TRUE)
SWW_med_dt <- median(SWW_min_dt, SWW_max_dt)

# SWW_base <- expand.grid(Start.DT = SWW_min_dt,
#                         End.DT = SWW_max_dt,
#                         Water.Company = unique(SWW_data$Water.Company)) 

generate_full_status <- function(df_site, min.DT, max.DT) {
  # Ensure proper ordering
  df_site <- df_site %>% arrange(Start.DT)
  
  # make leading start row
  if(!min.DT %in% min(df_site$Start.DT)){
    start_row <- slice(df_site, 1) %>% 
      mutate(next_start = Start.DT,
             End.DT = min.DT,
             Start.DT = NA)
  } else{
    start_row <- NULL
  }
  
  # Create "off" intervals in gaps between end[i] and start[i+1]
  off_intervals <- df_site %>%
    mutate(next_start = lead(Start.DT)) %>% 
    mutate(next_start = case_when(is.na(next_start) & End.DT != max.DT ~ max.DT,
                                  TRUE ~ next_start)) %>% 
    filter(!is.na(next_start) & End.DT < next_start) %>%
    bind_rows(., start_row) %>% 
    transmute(
      # Water.Company = first(Water.Company),
      Start.DT = End.DT,
      End.DT = next_start,
      Event.Type = "none"
    )
  
  # off_intervals <- off_intervals %>% 
  #   filter()
  
  # Combine original and "off" intervals
  df_site <- bind_rows(df_site, off_intervals) %>%
    arrange(Start.DT)
  
  # off_start_end <- df_site %>% 
  #   mutate(next_start = min.DT) %>% 
  #   filter(!is.na(next_start) & max.DT < next_start)
    
}


SWW_processed <- SWW_data %>%
  group_by(Water.Company) %>%
  group_modify(~ generate_full_status(.x, 
                                      min.DT = SWW_min_dt, 
                                      max.DT = SWW_max_dt)) %>%
  ungroup() 

SWW_constituencies <- constituencies %>% 
  st_filter(st_as_sf(SWW_data, crs = 4326, coords = c("lng", "lat")))



# plot them all
if(FALSE){
  SWW_processed %>%
    mutate(Status = factor(Event.Type, 
                           levels = c("spill", "maintenance", "none"),
                           labels = c("Spill", "Maintenance", "None"),
                           ordered = TRUE)) %>% 
    filter(Water.Company == "SWW0853") %>%
    ggplot(aes(y = 0, col = Status)) +
    geom_segment(aes(x = Start.DT, xend = End.DT), linewidth = 6) +
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
}

  

# discarded idea to have date slider displaying status by day because it's data intensive
# easier, cheaper, more readable to display a hist or some other data summary
# SWW_test_data <- expand.grid(Event.Date = seq(min(SWW_data$Start.Date, SWW_data$End.Date, na.rm = TRUE), 
#                                               max(SWW_data$Start.Date, SWW_data$End.Date, na.rm = TRUE),
#                                               by = "1 day"),
#                              Asset.ID = "South West",
#                              Water.Company = unique(SWW_data$Water.Company))

  
# pal <- colorFactor(palette = "Set1", domain = SWW_data$Water.Company)



#### Sites app ####

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
                       fillColor = "#57c2e2", color = "black", fill = TRUE,
                       layerId = ~Water.Company,
                       group = "points")
  })
  
  # start off unclicked
  clicked_site <- reactiveVal(NULL)
  
  # change to clicked if clicked
  observeEvent(input$sasmap_marker_click, {
    clicked_site(input$sasmap_marker_click$id)
    
    clicked_point <- SWW_data[SWW_data$Water.Company == clicked_site(), ]
    river_points <- SWW_data[SWW_data$Receiving.Water %in% clicked_point$Receiving.Water, ]
    
    leafletProxy("sasmap") %>%
      clearGroup("highlight") %>%
      clearGroup("river") %>%
      addCircleMarkers(
        data = river_points,
        lng = ~lng, lat = ~lat,
        radius = 5,
        color = "#fcd518",
        fillColor = "#fcd518",
        fillOpacity = 0.5,
        group = "river"
      ) %>% 
      addCircleMarkers(
        data = clicked_point,
        lng = ~lng, lat = ~lat,
        radius = 5,
        color = "#d3af00",
        fillColor = "#d3af00",
        fillOpacity = 1,
        group = "highlight"
      )
    
  })
  
  # highlight the point 
  
  
  # highlight points on the same river
  
  
  # render the barplot
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
    
    SWW_min_dt <- min(SWW_data$Start.DT, na.rm = TRUE)
    SWW_max_dt <- max(SWW_data$End.DT, na.rm = TRUE)
    SWW_med_dt <- median(SWW_min_dt, SWW_max_dt)
    
    SWW_dates <- data.frame(DT = c(SWW_min_dt,
                                   SWW_max_dt,
                                   SWW_med_dt),
                            stat = c("min", "max", "med")) %>% 
      mutate(label_date = as.Date(DT))
    
    ggplot() +
      geom_point(data = SWW_spills, aes(y = 0.5, x = Start.DT), size = 3, shape = 6, color = "#cf121d") +
      geom_segment(data = SWW_filtered, aes(y = 0, x = Start.DT, xend = End.DT, color = Status), linewidth = 6, show.legend = TRUE) +
      geom_text(data = SWW_dates, aes(y = -0.5, x = DT, label = label_date)) +
      ylim(c(-1,1)) +
      labs(x = "Time", y = "Category", title = unique(SWW_filtered$Water.Company)) +
      theme_void() +
      theme(axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            legend.position = "top",
            aspect.ratio = 0.3) +
      theme(plot.title = element_text(vjust=5)) +
      scale_colour_manual(
        values = c("#cf121d", "#585c62", "#1d9b7e"),
        drop = FALSE
      )
    
  })
  
}


shinyApp(ui, server)

#### Constituencies app ####

ui_constituency <- fluidPage(
  # titlePanel("SAS data vis map"),
  fluidRow(
    column(width = 3, uiOutput("summarytitle"), tableOutput("summarystat")),
    column(width = 7, leafletOutput("sasmap", height = "600px"))
  )
)

server_constituency <- function(input, output, session) {
  
  rv <- reactiveValues()
  rv$selected <- NULL
  
  # make the map
  output$sasmap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -4.3, lat = 50.55, zoom = 8) %>%
      addMarkers(lng = -3.5, lat = 50.7, popup = "Exeter") %>% 
      addPolygons(data = SWW_constituencies, color = "#fcd518", weight = 1,
                  fillOpacity = 0.4,
                  popup = ~PCON24NM,
                  layerId = ~PCON24NM,
                  group = "base")
    # addPolygons(data = SWW_constituencies, color = "blue", weight = 1,
    #             fillColor = "blue",
    #             fillOpacity = 0.5, 
    #             popup = ~PCON24NM,
    #             layerId = ~PCON24NM)
  })
  
  # start off unclicked
  # clicked_constituency <- reactiveVal(NULL)
  
  # change to clicked if clicked
  # observeEvent(input$sasmap_shape_click, {
  #   clicked_constituency(input$sasmap_shape_click$id)
  # })
  
  # highlight clicked polygon
  observeEvent(input$sasmap_shape_click, {
    clicked_id <- input$sasmap_shape_click$id
    
    clicked_poly <- SWW_constituencies[SWW_constituencies$PCON24NM == clicked_id, ]
    clicked_data <- sf::st_filter(SWW_data_sf, clicked_poly)
    
    # 
    spill_data <- clicked_data %>%
      filter(Event.Type == "spill") %>% 
      mutate(duration = End.DT - Start.DT)
    
    n_weeks <- floor(as.numeric(SWW_max_dt - SWW_min_dt, units = "weeks"))
    
    summary_data <- data.frame(
      N_events = paste(nrow(spill_data),
                       "spills since",
                       as.Date(SWW_min_dt),
                       # "spills over", 
                       # as.character(n_weeks), 
                       # "weeks", 
                       sep = " "),
      Hrs_spill = sum(spill_data$duration),
      N_sites = length(unique(clicked_data$Water.Company))
    ) %>% 
      mutate(
        # Hrs_per_site = as.numeric(Hrs_spill/N_sites, units = "hours"),
        Hrs_per_site = paste(floor(as.numeric(Hrs_spill/N_sites, units = "hours")),
                             "hours per site",
                             sep = " "),
        N_sites = paste(length(unique(clicked_data$Water.Company)),
                        "sites",
                        sep = " "),
        Hrs_spill = paste(floor(as.numeric(Hrs_spill, units = "hours")),
                          "hours over",
                          as.character(n_weeks),
                          "weeks",
                          sep = " "),
        Water_company = paste("Courtesy of", unique(spill_data$Asset.ID), "Water", sep = " "),
        MPs_email = paste("Send this to",
                          "YourLocalMP@email.com",
                          sep = " ")
      )
    
    
    # Clear any previous highlight, then add new one
    leafletProxy("sasmap") %>%
      clearGroup("highlight") %>%
      addPolygons(data = clicked_poly,
                  color = "#57c2e2", weight = 3,
                  fillColor = "#57c2e2", fillOpacity = 0.6,
                  layerId = "highlighted",
                  group = "highlight")
    
    
    # Produce table of summary stats
    output$summarytitle <- renderUI({
      h4(clicked_id)
    })
    
    output$summarystat <- renderTable({
      # Replace with the actual columns you want to show
      data.frame(
        # Statistic = names(summary_data),
        Value = as.character(t(summary_data)[,1]),
        row.names = NULL
      )
    }, colnames = FALSE)
    
    
  })
  
}


shinyApp(ui_constituency, server_constituency)
