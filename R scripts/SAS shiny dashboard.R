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
CSO_data <-
  xlsx::read.xlsx(
    here::here(dirname(here::here()), "Data", "CSO Database.xlsx"),
    sheetIndex = 1
  )

EDM_raw_data <- 
  read.csv(
    here::here(dirname(here::here()), "Data", "Sewage events 2025.csv")
  ) 

constituencies <- sf::read_sf(dsn = here::here(dirname(here::here()), "Data", "Constituencies_July_2024")) %>% 
  st_transform(shape, crs = 4326)

#### Prep the data generally ####
EDM_data <- EDM_raw_data %>% 
  rename(Asset.ID = Water.Company, Water.Company = Asset.ID) %>%
  filter(!if_all(everything(), ~ is.na(.) | . == "")) %>% 
  filter(!is.na(Longitude), !(Longitude == 0 & Latitude == 0)) %>% 
  rename(lng = Longitude, lat = Latitude) %>% 
  mutate(Start.DT = as.POSIXct(paste(Start.Date, Start.Time, sep = " "), format = "%d/%m/%Y %H:%M"),
         End.DT = as.POSIXct(paste(End.Date, End.Time, sep = " "), format = "%d/%m/%Y %H:%M"),
         across(c(Start.Date, End.Date), ~as.Date(.x, format = "%d/%m/%Y"))) %>% 
  filter(!is.na(End.DT), !is.na(Start.DT))

#### data prep for Constituencies app ####
# for constituencies
EDM_sf <- sf::st_as_sf(EDM_data, crs = 4326, coords = c("lng", "lat"))

EDM_min_dt <- min(EDM_data$Start.DT)
EDM_max_dt <- max(EDM_data$End.DT)
EDM_med_dt <- median(EDM_min_dt, EDM_max_dt)

# pre-process for speed
EDM_sf <- sf::st_join(EDM_sf, constituencies[, c("PCON24NM", "geometry")])

EDM_sf %>% 
  sf::st_drop_geometry() %>% 
  group_by(PCON24NM) %>% 
  summarise(n_comp = length(unique(Water.Company))) %>% View()

#### data prep for Sites app ####
# for intervals
EDM_min_dt <- min(EDM_data$Start.DT)
EDM_max_dt <- max(EDM_data$End.DT)
EDM_med_dt <- median(EDM_min_dt, EDM_max_dt)

# function to generate status in between events
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


# generate status in between events
EDM_full_status <- EDM_data %>%
  group_by(Asset.ID) %>%
  group_modify(~ generate_full_status(.x, 
                                      min.DT = EDM_min_dt, 
                                      max.DT = EDM_max_dt)) %>%
  ungroup() 




