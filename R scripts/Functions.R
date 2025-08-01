# functions

# Takes numbers and adds ordinal suffixes, e.g. 1 becomes 1st
ordinal_suffix <- function(x) {
  if (!is.numeric(x)) stop("Input must be numeric")
  suffix <- ifelse(x %% 100 %in% 11:13, "th",
                   switch(as.character(x %% 10),
                          "1" = "st",
                          "2" = "nd",
                          "3" = "rd",
                          "th"))
  paste0(x, suffix)
}

