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

# generate_fake_data
# fake EDM data for testing plots in shiny without too much overhead
# 
# Args:
#   df: A data frame containing the original EDM data.
#   n_records_per_asset: Number of fake records to generate per asset.
#
# Returns:
#   A data frame with fake data for each asset, including start and end dates.
#   Start and end dates are adjusted to fit within the specified date range.
#
# Example:
#   generate_fake_data(df, n_records_per_asset = 5)
generate_fake_data <- function(df, n_records_per_asset = 5) {
  
  # Get unique Asset.IDs from your dataframe
  unique_assets <- unique(df$Asset.ID)
  
  # Date range
  start_date <- EDM_min_dt
  end_date <- EDM_max_dt
  origin_rows <- data.frame(
    Asset.ID = unique_assets,
    Start.DT = start_date,
    Event.Type2 = factor("unaccounted", levels = c("spill", "none", "maintenance", "unaccounted"))
  )
  
  # Generate fake data
  fake_data <- map_dfr(unique_assets, function(asset_id) {
    
    # Generate random datetimes
    random_seconds <- runif(n_records_per_asset, 
                            min = as.numeric(start_date), 
                            max = as.numeric(end_date))
    
    
    data.frame(
      Asset.ID = asset_id,
      Start.DT = as.POSIXct(random_seconds, origin = "1970-01-01"),
      Event.Type2 = factor(sample(c("spill", "none", "maintenance"), 
                                  n_records_per_asset, 
                                  replace = TRUE,
                                  prob = c(0.2, 0.6, 0.2)),
                           levels = c("spill", "none", "maintenance", "unaccounted"))
    ) %>% 
      bind_rows(., origin_rows[origin_rows$Asset.ID == asset_id,]) %>% 
      arrange(Start.DT) %>% 
      mutate(End.DT = lead(Start.DT),
             End.DT = case_when(is.na(End.DT) ~ end_date,
                                TRUE ~ End.DT))
    
  }) 
  
  return(fake_data)
}
