######################################################################################################################
# Purpose: Build the dataset for the Create‑Your‑Own‑Plot section                                                    #
#          - Combines user‑selected service lines and custom groupings                                               #
#          - Aggregates across chosen time units (Day, Week, Month)                                                  #
#          - Produces unified dataset for plotting volume, capacity, availability, and utilization                   #
######################################################################################################################

# Create the function
build_custom_plot_data <- function(data_serviceline,
                                   aggregation_level,
                                   selected_lines,
                                   date_range,
                                   num_custom_groups,
                                   custom_group_names,
                                   custom_group_lines) {
  
  # Determine the time unit to aggregate by (Date, Week, or Month)
  group_var <- switch(aggregation_level,
                      "Days" = "Date",
                      "Weeks" = "Week",
                      "Months" = "Month")
  
  ################################################################################################
  # Step 1: Aggregate base service line data by selected time unit                               #
  #         - Filters to user‑selected service lines and date range                              #
  #         - Calculates mean Volume and Capacity per unit                                       #
  #         - Derives Availability and Utilization Rate                                          #
  ################################################################################################
  base <- data_serviceline |>
    dplyr::group_by(.data[[group_var]], ServiceLine) |>
    dplyr::summarise(
      Volume = mean(Volume, na.rm = TRUE),              # average volume per unit
      Capacity = mean(Capacity, na.rm = TRUE),          # average capacity per unit
      Available = Capacity - Volume,                    # capacity minus volume
      utilization_rate = dplyr::if_else(Capacity > 0, (Volume / Capacity) * 100, NA_real_), # % utilization
      .groups = "drop"
    ) |>
    dplyr::filter(
      ServiceLine %in% selected_lines,                  # keep only selected service lines
      .data[[group_var]] >= date_range[1],              # restrict to start date
      .data[[group_var]] <= date_range[2]               # restrict to end date
    ) |>
    dplyr::rename(Aggregation = .data[[group_var]])     # rename time unit column for consistent plotting
  
  ################################################################################################
  # Step 2: Build custom groups dynamically based on user input                                  #
  #         - Loops through each custom group                                                    #
  #         - Aggregates within each service line, then sums across lines per time unit          #
  #         - Assigns custom group name as ServiceLine                                           #
  ################################################################################################
  custom <- purrr::map_dfr(seq_len(num_custom_groups), function(i) {
    group_name <- custom_group_names[[i]]
    group_lines <- custom_group_lines[[i]]
    
    # Skip if group name or service lines are missing
    if (is.null(group_lines) || length(group_lines) == 0 || is.null(group_name) || group_name == "") {
      return(NULL)
    }
    
    # Step 2a: Aggregate each service line within the group
    grouped <- data_serviceline |>
      dplyr::filter(ServiceLine %in% group_lines,
                    .data[[group_var]] >= date_range[1],
                    .data[[group_var]] <= date_range[2]) |>
      dplyr::group_by(.data[[group_var]], ServiceLine) |>
      dplyr::summarise(
        Volume = mean(Volume, na.rm = TRUE),
        Capacity = mean(Capacity, na.rm = FALSE),
        .groups = "drop"
      )
    
    # Step 2b: Sum across all service lines in the group for each time unit
    grouped |>
      dplyr::group_by(.data[[group_var]]) |>
      dplyr::summarise(
        Volume = sum(Volume, na.rm = TRUE),              # total volume across group
        Capacity = sum(Capacity, na.rm = FALSE),         # total capacity across group
        Available = Capacity - Volume,                   # total available
        utilization_rate = dplyr::if_else(Capacity > 0, (Volume / Capacity) * 100, NA_real_), # % utilization
        .groups = "drop"
      ) |>
      dplyr::mutate(ServiceLine = group_name,            # assign custom group name
                    Aggregation = .data[[group_var]]) |> # rename time unit column
      dplyr::select(Aggregation, Volume, Capacity, Available, utilization_rate, ServiceLine)
  })
  
  ################################################################################################
  # Step 3: Combine base and custom group data into a single dataset                             #
  #         - Enables unified plotting and analysis                                              #
  ################################################################################################
  dplyr::bind_rows(base, custom)
}