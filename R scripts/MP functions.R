# MP data gathering functions

# get_constituency_data function
# This function queries the UK Parliament API to get constituency data based on a search term.
# It returns a list containing the constituency data.
# 
# Args:
#   constituency: A string representing the constituency to search for.
#
# Returns:
#   A list containing the constituency data, including the total results and items.
#
# Example:
#   get_constituency_data("Birmingham")
get_constituency_data <- function(constituency){
  res <- GET(
    url = "https://members-api.parliament.uk/api/Location/Constituency/Search",
    query = list(searchText = constituency, skip = 0, take = 20)
  )
  mp_data <- fromJSON(content(res, as = "text"), simplifyVector = FALSE)
  
  return(mp_data)
}

# get_member_info function
# This function extracts member information from a constituency JSON response.
# It retrieves the member ID and name based on the constituency data.
#
# Args:
#   constituency_json: A JSON object containing constituency data from the UK Parliament API.
#
# Returns:
#   A list containing the member ID and name, or NULL if the constituency is missing.
#
# Example:
#   get_member_info(constituency_json)
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

# get_member_data function
# This function retrieves member contact data from the UK Parliament API based on a member ID.
# It returns a JSON object containing the member's contact information.
#
# Args:
#   member_id: A string, or numeric value representing the member ID to query.
#
# Returns:
#   A JSON object containing the member's contact information, including email addresses.
# 
# Example:
#   get_member_data("1234")
# get member emails from IDs first so checks and cleaning are done afterwards
get_member_data <- function(member_id){
  res <- GET(
    url = paste0("https://members-api.parliament.uk/api/Members/", member_id, "/Contact")
  )
  mp_data <- fromJSON(content(res, as = "text"), simplifyVector = FALSE)
  
  return(mp_data)
}

get_member_email <- function(member_json) {
  emails <- map_chr(member_json$value, ~ .x$email %||% NA_character_)
  first_email <- emails[!is.na(emails)][1]
  if (is.na(first_email)) NA_character_ else first_email
}
