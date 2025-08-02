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

