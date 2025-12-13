######################################################################################################################                                                                                               #
# Purpose: Define a function to aggregate volume and capacity data into monthly averages for plotting                #
#           for the entire data set,  by TEAM level                                                                  #
######################################################################################################################

# Create function
summarize_by_team_level <- function(census_with_capacity) {
  # Define team-level groups
  team_groups <- list(
    "Team 1" = c("HMS9", "HMS10", "HMS11"),
    "Team 2" = c("HMS12", "Htransplant"),
    "HMS15" = c("HMS15Floor", "HMS15Stepdown")
  )
  
  # Aggregate grouped teams
  data_team <- purrr::map_dfr(names(team_groups), function(group_name) {
    teams <- team_groups[[group_name]]
    
    census_with_capacity |>
      dplyr::filter(Team %in% teams) |>
      dplyr::group_by(Date) |>
      dplyr::summarise(
        Team = group_name,
        Volume = sum(Volume, na.rm = TRUE),
        Capacity = sum(Capacity, na.rm = TRUE),
        Available = Capacity - Volume,
        Month = dplyr::first(Month),
        totalcapacity_calc = dplyr::first(totalcapacity_calc),
        ServiceLine = group_name,
        Team_Level = group_name,
        .groups = "drop"
      )
  })
  
  # Identify ungrouped teams
  grouped_teams <- unlist(team_groups)
  ungrouped_data <- census_with_capacity |>
    dplyr::filter(!(Team %in% grouped_teams))
  
  # Combine grouped and ungrouped
  data_team <- dplyr::bind_rows(data_team, ungrouped_data) |>
    dplyr::arrange(Date, Team)
  
  # Set plotting order
  team_order <- c("HMS1", "HMS2", "HMS3", "HMS4", "HMS5", 
                  "HMS6", "HMS7", "HMS8", "Team 1", "Team 2", 
                  "HMS13", "HMS14", "HMS15", "HMS16", "HMS17", "HMS18", "HMS19", "HMS20", 
                  "HTT", "MED1", "MED2", "MED3", "MED4", "ACE1", "ACE2", "ACE3", 
                  "Medicine\r\nConsults", "NEWMED\r\nCONSULTS", "Med Consults Combined", 
                  "Addiction\r\nMed", "ONC1", "ONC2", "ONC3", "DAY", "SWING", "NIGHT", 
                  "AVAILCAP", "IMTOTAL", "CON")
  
  data_team <- data_team |>
    dplyr::mutate(Team = factor(Team, levels = team_order))
  
  # Monthly aggregation
  data_team_month <- data_team |>
    dplyr::group_by(Month, Team) |>
    dplyr::summarise(
      Volume = mean(Volume, na.rm = TRUE),
      Capacity = mean(Capacity, na.rm = TRUE),
      Available = Capacity - Volume,
      utilization_rate = (Volume / Capacity) * 100,
      .groups = "drop"
    ) |>
    dplyr::filter(!Team %in% c("HTT", "Med Consults Combined", "IMTOTAL", "AVAILCAP", "DAY", "SWING", "NIGHT"))
  
  # Apply overrides for known fixed capacities
  data_team_month <- data_team_month |>
    dplyr::mutate(Capacity = dplyr::case_when(
      Team == "Addiction\r\nMed" ~ 16,
      Team == "Medicine\r\nConsults" ~ 48,
      TRUE ~ Capacity
    )) |>
    dplyr::mutate(
      Available = Capacity - Volume,
      utilization_rate = (Volume / Capacity) * 100
    )
  
  # Return final data sets
  return(list(
    data_team = data_team,
    data_team_month = data_team_month
  ))
}