# =============================================================================
# Title:        MP emails for constituencies dash
# Description:  query APIs to get MP email addresses and collate
# Author:       Margaret Bolton 
# Created:      2025-06-23
# Dependencies: tidyverse, sf
# Inputs:       List of constituencies
# Outputs:      DF of MP names, IDs, and emails
# =============================================================================

library(tidyverse)
library(sf)
library(httr)
library(jsonlite)
library(purrr)

# list of constituencies to work with
constituencies_raw <- sf::read_sf(dsn = here::here(project_root, "Data", "Constituencies_July_2024"))
constituencies <- constituencies_raw %>% 
  st_transform(shape, crs = 4326) %>% 
  mutate(PCON24NM = case_when(str_detect(PCON24NM, "Glyndwr") ~ str_replace_all(PCON24NM, "Glyndwr", "Glyndŵr"),
                              TRUE ~ PCON24NM))

# ready to add mp info
constituency_data <- data.frame(PCON24NM = constituencies$PCON24NM, 
                                MemberID = NA,
                                MemberName = NA,
                                MemberEmail = NA)

# query constituency data from Developer Hub API
list_constituency_data <- lapply(constituency_data$PCON24NM, 
                                 get_constituency_data)

# extract member information (ID, name) from constituency data
constituency_data <- constituency_data %>%
  mutate(result = map(list_constituency_data, get_member_info),
         MemberID = map_int(result, "mp_id"),
         MemberName = map_chr(result, "mp_name")
  ) %>%
  select(-result)


# query member details from Developer hub API
list_member_data <- lapply(constituency_data$MemberID, 
                                 get_member_data)

# now finally extract email and save data
constituency_data <- constituency_data %>% 
  mutate(MemberEmail = sapply(list_member_data, get_member_email),
         MemberEmail = trimws(MemberEmail))


write.csv(constituency_data, here::here(project_root, "Outputs", "MP_data.csv"))

constituency_data <-read.csv(here::here(project_root, "Outputs", "MP_data.csv"))
