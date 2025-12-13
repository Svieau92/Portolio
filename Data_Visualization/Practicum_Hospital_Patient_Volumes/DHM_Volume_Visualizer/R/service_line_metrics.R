######################################################################################################################
# Purpose: Calculate summary metrics for a given service line over a user-selected look-back period                  #
#          - Aggregates daily volume and capacity                                                                    #
#          - Derives utilization rate, days over 90%, longest streak, and days at 100%                               #
######################################################################################################################

# Create function
getServiceLineMetrics <- function(service_line_name, census_df, look_back_period) {
  
  # Map look-back period to number of days
  look_back_days <- switch(look_back_period,
                           "1 Week"   = 7, 
                           "2 Weeks"  = 14, 
                           "1 Month"  = 30,
                           "3 Months" = 90, 
                           "6 Months" = 180, 
                           "1 Year"   = 365, 
                           30)  # default fallback
  
  # Cutoff date for filtering
  cutoff <- max(census_df$Date, na.rm = TRUE) - look_back_days
  
  # Filter data for this service line and look-back window
  df <- census_df |> 
    filter(ServiceLine == service_line_name, Date >= cutoff)
  
  # Aggregate daily totals
  daily <- df |>  
    group_by(Date, ServiceLine) |>  
    summarise(
      Volume   = sum(Volume, na.rm = TRUE),   # total daily volume
      Capacity = sum(Capacity, na.rm = TRUE), # total daily capacity
      .groups  = "drop"
    ) |>  
    mutate(
      utilization_rate = Volume / Capacity * 100, # % utilization
      over_90 = utilization_rate > 90,            # flag days > 90%
      at_100  = utilization_rate == 100           # flag days at 100%
    )
  
  # Check if capacity data is missing or zero
  has_capacity <- all(is.na(daily$Capacity)) || sum(daily$Capacity, na.rm = TRUE) == 0 # TRUE if every capacity value is NA OR if the sum of all capacity values is zero
  
  # Always calculate average daily volume (safe even if capacity is missing)
  avg_volume <- round(mean(daily$Volume, na.rm = TRUE), 1)
  
  # Calculate metrics depending on whether capacity exists
  if (has_capacity) {
    
    # If no valid capacity data, mark all capacity-related metrics as "N/A"
    avg_capacity   <- "N/A"   # cannot compute average available capacity
    avg_pct        <- "N/A"   # cannot compute utilization percentage
    days_over_90   <- "N/A"   # cannot count days over 90% utilization
    longest_streak <- "N/A"   # cannot compute streaks
    days_at_100    <- "N/A"   # cannot count days at full utilization
  } else {
    
    # If capacity exists, calculate meaningful metrics
    avg_capacity <- round(mean(daily$Capacity - daily$Volume, na.rm = TRUE), 1) # average available capacity = capacity minus volume
    
    avg_pct <- round((sum(daily$Volume, na.rm = TRUE) / sum(daily$Capacity, na.rm = TRUE)) * 100, 1) # overall utilization percentage across the look-back period
    
    days_over_90 <- sum(daily$over_90, na.rm = TRUE) # number of days where utilization exceeded 90%
    
    # Run-length encoding to find longest streak of days > 90%
    rle_obj <- rle(daily$over_90)
    longest_streak <- if (any(rle_obj$values, na.rm = TRUE)) {
      max(rle_obj$lengths[rle_obj$values], na.rm = TRUE)
    } else { 0 }
    
    days_at_100 <- sum(daily$at_100, na.rm = TRUE)
  }
  
  # Return metrics as a list
  list(
    avg_volume     = avg_volume,
    avg_capacity   = avg_capacity,
    avg_pct        = avg_pct,
    days_over_90   = days_over_90,
    longest_streak = longest_streak,
    days_at_100    = days_at_100
  )
}