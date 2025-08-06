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

constituencies_raw <- sf::read_sf(dsn = here::here(project_root, "Data", "Constituencies_July_2024")) %>% 
  mutate(PCON24NM = case_when(str_detect(PCON24NM, "Glyndwr") ~ str_replace_all(PCON24NM, "Glyndwr", "Glyndŵr"),
                              TRUE ~ PCON24NM)) 
constituencies_sf <- constituencies_raw %>% 
  st_transform(shape, crs = 4326) %>% 
  mutate(PCON24NM = as.factor(PCON24NM))

MP_data <- read.csv(here::here(project_root, "Outputs", "MP_data.csv"))

#### Prep the data generally ####
# shed missing data, format dates
EDM_data <- EDM_raw_data %>% 
  rename(Asset.ID = Water.Company, Water.Company = Asset.ID) %>%
  mutate(Water.Company.Name = paste(Water.Company, "Water", sep = " ")) %>% 
  filter(!if_all(everything(), ~ is.na(.) | . == "")) %>% 
  filter(!is.na(Longitude), !(Longitude == 0 & Latitude == 0)) %>% 
  rename(lng = Longitude, lat = Latitude) %>% 
  mutate(Start.DT = as.POSIXct(paste(Start.Date, Start.Time, sep = " "), format = "%d/%m/%Y %H:%M"),
         End.DT = as.POSIXct(paste(End.Date, End.Time, sep = " "), format = "%d/%m/%Y %H:%M"),
         across(c(Start.Date, End.Date), ~ as.Date(.x, format = "%d/%m/%Y"))) %>% 
  filter(!is.na(End.DT), !is.na(Start.DT)) %>% 
  mutate(duration = difftime(End.DT, Start.DT, units = "hours"),
         Event.Type2 = case_when(sign(duration) == -1 ~ "unaccounted",
                                 TRUE ~ Event.Type))
  
#### data prep for apps ####
# for constituencies
constituencies_exploded <- sf::st_cast(constituencies_sf, "POLYGON")

# pre-processing for later
EDM_sf <- sf::st_as_sf(EDM_data, crs = 4326, coords = c("lng", "lat")) %>% 
  sf::st_join(constituencies_sf[, c("PCON24NM", "geometry")]) %>% 
  mutate(PCON24NM = case_when(is.na(PCON24NM) ~
                                constituencies_exploded$PCON24NM[sf::st_nearest_feature(geometry, constituencies_exploded)],
                              TRUE ~ PCON24NM),
         PCON24NM = factor(PCON24NM, levels = unique(constituencies_sf$PCON24NM))) 

save_with_difftime(EDM_sf, savename = "EDM_sf.gpkg")

CSO_sf <- sf::st_as_sf(CSO_data, crs = 4326, coords = c("Longitude", "Latitude")) %>% 
  sf::st_join(constituencies_sf[, c("PCON24NM", "geometry")]) %>% 
  mutate(PCON24NM = case_when(is.na(PCON24NM) ~
                                constituencies_exploded$PCON24NM[sf::st_nearest_feature(geometry, constituencies_exploded)],
                              TRUE ~ PCON24NM),
         PCON24NM = factor(PCON24NM, levels = unique(constituencies_sf$PCON24NM)))

##### Basic date info #####
EDM_min_dt <- min(EDM_data$Start.DT)
EDM_max_dt <- max(EDM_data$End.DT)
EDM_wks_dt <- difftime(EDM_max_dt, EDM_min_dt, units = "weeks")

# TODO: will not need to be saved when date and duration is a user input, see issue #12
summary_dates <- data.frame(Main_End.Date = EDM_max_dt,
                            Main_Start.Date = EDM_min_dt,
                            Main_duration = EDM_wks_dt)

write.csv(summary_dates, here::here(project_root, "Outputs", "summary_dates.csv"))

##### summarise data for constituencies app ####

# Take constituencies and add summary data
CSO_summary <- CSO_sf %>% 
  sf::st_drop_geometry() %>% 
  group_by(PCON24NM) %>% 
  summarise(N_sites = length(unique(Asset.ID))) %>% 
  complete(PCON24NM) %>% 
  mutate(N_sites = replace_na(N_sites, 0))

EDM_summary <- EDM_sf %>% 
  sf::st_drop_geometry() %>% 
  filter(Event.Type2 ==  "spill") %>% 
  group_by(PCON24NM) %>% 
  summarise(N_spills  = n(),
            Hrs_spill = sum(duration),
            Companies = stringr::str_flatten_comma(unique(Water.Company.Name), last = " and ")) %>% 
  complete(PCON24NM) %>% 
  mutate(N_spills = replace_na(N_spills, 0),
         Hrs_spill = replace_na(Hrs_spill, as.difftime(0, units = "hours")))

constituencies_sf <- constituencies_sf %>% 
  left_join(MP_data, by = join_by(PCON24NM), 
            relationship = "one-to-one",
            unmatched = "error") %>% 
  left_join(CSO_summary, by = join_by(PCON24NM), 
            relationship = "one-to-one",
            unmatched = "error") %>% 
  left_join(EDM_summary, by = join_by(PCON24NM), 
            relationship = "one-to-one",
            unmatched = "error") %>% 
  mutate(Hrs_site = Hrs_spill/N_sites,
         Hrs_site_week = Hrs_site/as.numeric(summary_dates$Main_duration))


constituencies_sf <- constituencies_sf %>%
  mutate(across(where(is.difftime), ~as.numeric(.x, units = "hours")))

# constituencies_sf <- constituencies_sf %>% 
  # mutate(cloropleth_fill = case_when(Hrs_site == 0  ~ NA_real_,
                                      # TRUE ~ Hrs_site))

sf::st_write(constituencies_sf, here::here(project_root, "Outputs", "constituency_sf.gpkg"),
             delete_dsn = TRUE)

##### Intervals for sites app #####
# generate status in between events
EDM_full_status <- EDM_data %>%
  group_by(Asset.ID) %>%
  group_modify(~ generate_full_status(.x, 
                                      min.DT = EDM_min_dt, 
                                      max.DT = EDM_max_dt)) %>%
  ungroup() 

EDM_full_status <- EDM_full_status %>% 
  mutate(duration = difftime(End.DT, Start.DT, units = "hours"),
         Event.Type2 = case_when(sign(duration) == -1 ~ "unaccounted",
                                 TRUE ~ Event.Type))

write.csv(EDM_full_status, here::here(project_root, "Outputs", "EDM_full_status.csv"))
# EDM_full_status <- read.csv(here::here(project_root, "Outputs", "EDM_full_status.csv"))

##### Generate fake data for testing #####
EDM_fake_data <- CSO_sf %>% 
  sf::st_drop_geometry() %>% 
  select(Asset.ID, Water.Company, PCON24NM) %>% 
  right_join(generate_fake_data(CSO_data), join_by(Asset.ID))

write.csv(EDM_fake_data, here::here(project_root, "Outputs", "EDM_fake_data.csv"))
