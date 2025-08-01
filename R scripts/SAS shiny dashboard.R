# =============================================================================
# Title:        EIB25 Surfers Against Sewage Shiny dashboard
# Description:  Shiny dashboard for EIB Grand Challenge
# Author:       Margaret Bolton 
# Created:      2025-06-23
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
library(showtext)
library(ggplot2)
sysfonts::font_add("HVDposter", "C:/Windows/Fonts/HVD Poster.ttf")  # adjust path if needed
sysfonts::font_add("HVD_Poster_Clean", "C:/Windows/Fonts/HVD_Poster_Clean.ttf")  # adjust path if needed
sysfonts::font_add("Roboto", "C:/Windows/Fonts/Roboto-VariableFont_wdth,wght.ttf")  # adjust path if needed
# library(rsconnect)

#### setup ####
project_root <- dirname(here::here())
showtext::showtext_auto()

CSO_data <-
  xlsx::read.xlsx(
    here::here(project_root, "Data", "CSO Database.xlsx"),
    sheetIndex = 1
  )

EDM_raw_data <- 
  read.csv(
    here::here(project_root, "Data", "Sewage events 2025.csv")
  ) 

constituencies_raw <- sf::read_sf(dsn = here::here(project_root, "Data", "Constituencies_July_2024"))
constituencies <- constituencies_raw %>% 
  st_transform(shape, crs = 4326) %>% 
  mutate(PCON24NM = case_when(str_detect(PCON24NM, "Glyndwr") ~ str_replace_all(PCON24NM, "Glyndwr", "Glyndŵr"),
                              TRUE ~ PCON24NM))

#### Prep the data generally ####
EDM_data <- EDM_raw_data %>% 
  rename(Asset.ID = Water.Company, Water.Company = Asset.ID) %>%
  filter(!if_all(everything(), ~ is.na(.) | . == "")) %>% 
  filter(!is.na(Longitude), !(Longitude == 0 & Latitude == 0)) %>% 
  rename(lng = Longitude, lat = Latitude) %>% 
  mutate(Start.DT = as.POSIXct(paste(Start.Date, Start.Time, sep = " "), format = "%d/%m/%Y %H:%M"),
         End.DT = as.POSIXct(paste(End.Date, End.Time, sep = " "), format = "%d/%m/%Y %H:%M"),
         across(c(Start.Date, End.Date), ~as.Date(.x, format = "%d/%m/%Y"))) %>% 
  filter(!is.na(End.DT), !is.na(Start.DT)) %>% 
  mutate(duration = floor(difftime(End.DT, Start.DT, units = "hours")),
         Event.Type2 = case_when(sign(duration) == -1 ~ "unaccounted",
                                 TRUE ~ Event.Type))
  
#### data prep for apps####
# for constituencies
constituencies_exploded <- sf::st_cast(constituencies, "POLYGON")

# pre-processing constituencies for speed later
EDM_sf <- sf::st_as_sf(EDM_data, crs = 4326, coords = c("lng", "lat")) %>% 
  sf::st_join(constituencies[, c("PCON24NM", "geometry")]) %>% 
  mutate(PCON24NM = case_when(is.na(PCON24NM) ~
                                constituencies_exploded$PCON24NM[sf::st_nearest_feature(geometry, constituencies_exploded)],
                              TRUE ~ PCON24NM)) 

CSO_sf <- sf::st_as_sf(CSO_data, crs = 4326, coords = c("Longitude", "Latitude")) %>% 
  sf::st_join(constituencies[, c("PCON24NM", "geometry")]) %>% 
  mutate(PCON24NM = case_when(is.na(PCON24NM) ~
                                constituencies_exploded$PCON24NM[sf::st_nearest_feature(geometry, constituencies_exploded)],
                              TRUE ~ PCON24NM))


# for intervals
EDM_min_dt <- min(EDM_data$Start.DT)
EDM_max_dt <- max(EDM_data$End.DT)
EDM_med_dt <- median(EDM_min_dt, EDM_max_dt)

# generate status in between events
EDM_full_status <- EDM_data %>%
  group_by(Asset.ID) %>%
  group_modify(~ generate_full_status(.x, 
                                      min.DT = EDM_min_dt, 
                                      max.DT = EDM_max_dt)) %>%
  ungroup() 

EDM_full_status <- EDM_full_status %>% 
  mutate(duration = floor(difftime(End.DT, Start.DT, units = "hours")),
         Event.Type2 = case_when(sign(duration) == -1 ~ "unaccounted",
                                 TRUE ~ Event.Type))

write.csv(EDM_full_status, here::here(project_root, "Outputs", "EDM_full_status"))
EDM_full_status <- read.csv(here::here(project_root, "Outputs", "EDM_full_status"))

