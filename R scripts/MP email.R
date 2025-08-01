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

# query the constituency API first so tidying can be done separately
get_constituency_data <- function(constituency){
  res <- GET(
    url = "https://members-api.parliament.uk/api/Location/Constituency/Search",
    query = list(searchText = constituency, skip = 0, take = 20)
  )
  mp_data <- fromJSON(content(res, as = "text"), simplifyVector = FALSE)
  
  return(mp_data)
  
}

list_constituency_data <- lapply(constituency_data$PCON24NM, 
                                 get_constituency_data)

# tidy up multiple matches or missing values and get member IDs and names
# TODO: horrible to read
get_member_info <- function(constituency_json){
  constituency <- constituency_json$resultContext
  constituency <- sub('.*matching \\"(.*?)\\".*', '\\1', constituency)
  
  if(constituency_json$totalResults == 1){
    if(length(constituency_json$items) != 1){warning(paste0("WAH length doesn't match for ", constituency))}
    mp_id <- constituency_json$items[[1]]$value$currentRepresentation$member$value$id
    mp_name <- constituency_json$items[[1]]$value$currentRepresentation$member$value$nameDisplayAs
    
    
  } else if(constituency_json$totalResults > 1){
    item_names <- sapply(constituency_json$items, function(x){x$value$name})
    if(!any(item_names %in% constituency)){
      warning(paste0("Big wah no name for ", constituency))
    } else if(constituency_json$items[[which(item_names %in% constituency)]]$value$name != constituency){
      warning(paste0("wah names don't match for ", constituency))
    } else {
      mp_id <- constituency_json$items[[which(item_names %in% constituency)]]$value$currentRepresentation$member$value$id
      mp_name <- constituency_json$items[[which(item_names %in% constituency)]]$value$currentRepresentation$member$value$nameDisplayAs
      
    }
    
  } else {
    warning(paste0("uh oh missing ", constituency))
    return(NULL)
  }
  return(list("mp_id" = mp_id, "mp_name" = mp_name))
}


constituency_data <- constituency_data %>%
  mutate(result = map(list_constituency_data, get_member_info),
         MemberID = map_int(result, "mp_id"),
         MemberName = map_chr(result, "mp_name")
  ) %>%
  select(-result)

# get member emails from IDs first so checks and cleaning are done afterwards
get_member_data <- function(member_id){
  res <- GET(
    url = paste0("https://members-api.parliament.uk/api/Members/", member_id, "/Contact")
  )
  mp_data <- fromJSON(content(res, as = "text"), simplifyVector = FALSE)
  
  return(mp_data)
}

list_member_data <- lapply(constituency_data$MemberID, 
                                 get_member_data)

# now finally extract email and save data
get_member_email <- function(member_json) {
  emails <- map_chr(member_json$value, ~ .x$email %||% NA_character_)
  first_email <- emails[!is.na(emails)][1]
  if (is.na(first_email)) NA_character_ else first_email
}

constituency_data <- constituency_data %>% 
  mutate(MemberEmail = sapply(list_member_data, get_member_email),
         MemberEmail = trimws(MemberEmail))


write.csv(constituency_data, here::here(project_root, "Outputs", "MP_data.csv"))

constituency_data <-read.csv(here::here(project_root, "Outputs", "MP_data.csv"))
