######################################################################################################################
# Purpose: Calculate summary metrics for a given team within a service line over a user-selected look-back period    #
######################################################################################################################

# Create function
getTeamMetrics <- function(service_line_name, team_name, census_df, look_back_period) {
  
  # Map look-back period string to number of days
  look_back_days <- switch(look_back_period,
                           "1 Week"   = 7,
                           "2 Weeks"  = 14,
                           "1 Month"  = 30,
                           "3 Months" = 90,
                           "6 Months" = 180,
                           "1 Year"   = 365,
                           30)  # default fallback if input doesn't match
  
  # Cutoff date for filtering based on look-back window
  cutoff <- max(census_df$Date, na.rm = TRUE) - look_back_days
  
  # Filter data for the given service line, team, and date range
  df <- census_df |>
    dplyr::filter(ServiceLine == service_line_name,
                  Team == team_name,
                  Date >= cutoff)
  
  # Aggregate daily totals for volume and capacity
  daily <- df |>
    dplyr::group_by(Date) |>
    dplyr::summarise(
      Volume   = sum(Volume, na.rm = TRUE),   # total daily volume
      Capacity = sum(Capacity, na.rm = TRUE), # total daily capacity
      .groups  = "drop"
    ) |>
    dplyr::mutate(
      utilization_rate = Volume / Capacity * 100, # % utilization
      over_90 = utilization_rate > 90,            # flag days > 90% utilization
      at_100  = utilization_rate == 100           # flag days at 100% utilization
    )
  
  # Check if capacity data is missing or zero
  has_capacity <- all(is.na(daily$Capacity)) || sum(daily$Capacity, na.rm = TRUE) == 0
  
  # Always calculate average daily volume (safe even if capacity is missing)
  avg_volume <- round(mean(daily$Volume, na.rm = TRUE), 1)
  
  if (has_capacity) {
    # If no valid capacity data, mark all capacity-related metrics as "N/A"
    avg_capacity   <- "N/A"
    avg_pct        <- "N/A"
    days_over_90   <- "N/A"
    longest_streak <- "N/A"
    days_at_100    <- "N/A"
  } else {
    # If capacity exists, calculate meaningful metrics
    
    # Average available capacity = mean of (capacity - volume)
    avg_capacity <- round(mean(daily$Capacity - daily$Volume, na.rm = TRUE), 1)
    
    # Overall utilization percentage across the look-back period
    avg_pct <- round((sum(daily$Volume, na.rm = TRUE) / sum(daily$Capacity, na.rm = TRUE)) * 100, 1)
    
    # Number of days where utilization exceeded 90%
    days_over_90 <- sum(daily$over_90, na.rm = TRUE)
    
    # Run-length encoding to find longest streak of days > 90%
    rle_obj <- rle(daily$over_90)
    longest_streak <- if (any(rle_obj$values, na.rm = TRUE)) {
      max(rle_obj$lengths[rle_obj$values], na.rm = TRUE)
    } else { 0 }
    
    # Number of days at full utilization (100%)
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