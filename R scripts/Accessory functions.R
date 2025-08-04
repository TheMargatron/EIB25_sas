# functions

# ordinal_suffix function
# Appends ordinal suffix (st, nd, rd, th) to a number
# 
# Args:
#   x: A numeric vector of length 1
#
# Returns:
#   A character vector with ordinal suffix appended (e.g., "1st", "2nd")
#
# Example:
#   ordinal_suffix(1)
#   # [1] "1st"
ordinal_suffix <- function(x){
  if (!is.numeric(x)) stop("Input must be numeric")
  suffix <- ifelse(x %% 100 %in% 11:13, "th",
                   switch(as.character(x %% 10),
                          "1" = "st",
                          "2" = "nd",
                          "3" = "rd",
                          "th"))
  paste0(x, suffix)
}

# date_in_text function
# Convert date to formatted string for use in text
# 
# Args:
#   x: A Date object
#
# Returns:
#   A character string with the date formatted as "Month day_suffix"
# Example:
#   date_in_text(as.Date("2023-10-01"))
#   # [1] "October 1st"
date_in_text <- function(x){
  if(!lubridate::is.Date(x)) stop("Input must be date")
  text_date <- paste(format(x, "%B"),
                     ordinal_suffix(as.numeric(format(x, "%d"))),
                     sep = " ")
  
  return(text_date)
}

# cloro_colour function 
# Generates custom colour palette for cloropleth maps
# For use with colorNumeric
#
# Args:
#   x: A numeric vector of values to map to colours
#   threshold: A numeric value to determine the maximum for the colour ramp
#
# Returns:
#   A character vector of colours corresponding to the input values
#
# Example:
#   cloro_colour(c(0, 0.5, 1))
#   # [1] "#23b99e" "#f0515a" "#f0515a"
cloro_colour <- function(x, threshold = 0.5) {
  ramp <- colorRamp(c("#ffdc32", "#f0515a"))
  sapply(x, function(val) {
    if (is.na(val)) {
      return("#dbdbda") 
    } else if (val == 0) {
      return("#23b99e")
    } else if (val > threshold) {
      return("#f0515a") 
    } else {
      rgb(ramp(val / threshold)/255)
    }
  })
}

# save_with_difftime function
# Save sf objects with difftime columns as numeric values with units
#
# Args:
#   x: An sf object with difftime columns
#   savename: A string for the output file name
#   units: A string specifying the units for the difftime conversion (default is "hours")
#
# Returns:
#   Saves the sf object to a specified file path with difftime columns converted to numeric
save_with_difftime <- function(x, savename, units = "hours"){
  x <- x %>%
    mutate(across(where(is.difftime), ~as.numeric(.x, units = units)))
  
  sf::st_write(x, here::here(project_root, "Outputs", savename),
               delete_dsn = TRUE)
}

