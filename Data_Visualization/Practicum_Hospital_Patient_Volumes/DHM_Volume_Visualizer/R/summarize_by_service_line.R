######################################################################################################################                                                                                               #
# Purpose: Define a function to aggregate volume and capacity data into monthly averages for plotting                #
#           for the entire data set,  by service line level                                                          #
######################################################################################################################

# Create the function
summarize_by_service_line <- function(census_with_capacity) {
  # Define service line groups
  team_groups <- list(
    "ACE" = c("ACE1", "ACE2", "ACE3"),
    "Medicine Consults" = c("Medicine\r\nConsults", "NEWMED\r\nCONSULTS"),
    "Addiction Medicine" = c("AddictionMed", "Addiction\r\nMed", "Addiction Medicine"),
    "General Medicine" = c("HMS1", "HMS2", "HMS3", "HMS4", "HMS5", "HMS6", "HMS8", "HMS9", "HMS10", "HMS11", "HMS12", "HMS13", "HMS14", "HMS15Floor", "HMS16", "HMS17", "HMS18", "HMS19", "HMS20", "HTT", "MED1", "MED2", "MED3", "MED4"),
    "Stepdown" = c("HMS15Stepdown"),
    "HMS7" = c("HMS7"),
    "Med Oncology" = c("ONC1", "ONC2", "ONC3"),
    "Transplant" = c("Htransplant"),
    "Day" = c("DAY"),
    "Swing" = c("SWING"),
    "Night" = c("NIGHT")
  )
  
  # Aggregate by service line
  data_serviceline <- purrr::map_dfr(names(team_groups), function(group_name) {
    teams <- team_groups[[group_name]]
    
    census_with_capacity |>
      dplyr::filter(Team %in% teams) |>
      dplyr::group_by(Date) |>
      dplyr::summarise(
        Team = group_name,
        Volume = sum(Volume, na.rm = TRUE),
        Capacity = sum(Capacity, na.rm = TRUE),
        Available = Capacity - Volume,
        Date = Date,
        Week = Week,
        Month = dplyr::first(Month),
        totalcapacity_calc = dplyr::first(totalcapacity_calc),
        ServiceLine = group_name,
        Team_Level = group_name,
        .groups = "drop"
      )
  })
  
  # Monthly aggregation
  data_serviceline_month <- data_serviceline |>
    dplyr::group_by(Month, ServiceLine) |>
    dplyr::summarise(
      Volume = mean(Volume, na.rm = TRUE),
      Capacity = mean(Capacity, na.rm = TRUE),
      Available = Capacity - Volume,
      utilization_rate = (Volume / Capacity) * 100,
      .groups = "drop"
    )
  
  # Set plotting order
  ordered_levels <- c("Addiction Medicine", "ACE", "General Medicine", "HMS7", "Stepdown",
                      "Med Oncology", "Medicine Consults", "Transplant", "Day", "Swing", "Night",
                      "IMTOTAL", "AVAILCAP")
  
  data_serviceline_month <- data_serviceline_month |>
    dplyr::mutate(ServiceLine = factor(ServiceLine, levels = ordered_levels))
  
  # Return both daily and monthly datasets
  return(list(
    data_serviceline = data_serviceline,
    data_serviceline_month = data_serviceline_month
  ))
}