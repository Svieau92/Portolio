######################################################################################################################                                                                                               #
# Purpose: Define a function to aggregate volume and capacity data into monthly averages for plotting                #
#           for the entire academic year                                                                             #
######################################################################################################################

# Create function
summarize_monthly_data <- function(combined_data, data_daily_caps) {
  
  # Perform interval join
  merged_data <- fuzzyjoin::fuzzy_left_join(
    combined_data,
    data_daily_caps,
    by = c("Date" = "StartDate", "Date" = "EndDate"),
    match_fun = list(`>=`, `<=`)
  )
  
  # Filter and derive time variables
  merged_data <- merged_data |>
    dplyr::filter(Date >= as.Date("2021-01-01")) |>
    dplyr::mutate(
      Week = lubridate::floor_date(Date, unit = "week", week_start = 1),
      Month = lubridate::floor_date(Date, unit = "month")
    ) |>
    dplyr::select(Date, Week, Month, dplyr::everything())
  
  # Convert AY to factor
  merged_data$AY <- factor(merged_data$AY)
  
  # Convert character/logical columns to numeric
  merged_data <- merged_data |>
    dplyr::mutate(across(
      where(~ (is.character(.) || is.logical(.)) && !all(is.na(.))),
      ~ suppressWarnings(as.numeric(.))
    ))
  
  # Monthly summary
  data_months <- merged_data |>
    dplyr::group_by(Month, AY) |>
    dplyr::summarise(
      average_IM = mean(IMTOTAL, na.rm = TRUE),
      average_cap = mean(totalcapacity_calc, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      Month_label = format(Month, "%b"),
      Month_label = factor(Month_label, levels = month.abb),
      Available = average_cap - average_IM,
      utilization_rate = (average_IM / average_cap) * 100
    )
  
  # Academic year month ordering
  month_levels_academic <- c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
                             "Jan", "Feb", "Mar", "Apr", "May", "Jun")
  
  data_months <- data_months |>
    dplyr::mutate(Month_label = factor(Month_label, levels = month_levels_academic))
  
  return(data_months)
}