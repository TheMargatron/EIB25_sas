# Data processing

# generate_full_status function
# This function takes a data frame of site events and infers a complete set of intervals,
# including "off" periods where no events are occurring.
#
# Args:
#   df_site: A data frame containing site events with columns Start.DT, End.DT, and Event.Type.
#   min.DT: The minimum date-time to consider for the start of the intervals.
#   max.DT: The maximum date-time to consider for the end of the intervals.
# 
# Returns:
#   A partially ordered data frame with complete intervals.
# 
# Example:
#   generate_full_status(df_site, min.DT, max.DT)
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
      Start.DT = End.DT,
      End.DT = next_start,
      Event.Type = "none"
    )
  
  # Combine original and "off" intervals
  df_site <- bind_rows(df_site, off_intervals) 
}