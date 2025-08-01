# functions

# Takes numbers and adds ordinal suffixes, e.g. 1 becomes 1st
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

date_in_text <- function(x){
  if(!lubridate::is.Date(x)) stop("Input must be date")
  text_date <- paste(format(x, "%B"),
                     ordinal_suffix(as.numeric(format(x, "%d"))),
                     sep = " ")
  
  return(text_date)
}
