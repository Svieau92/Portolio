######################################################################################################################                                                                                               #
# Purpose: Define a function to aggregate volume and capacity data into monthly averages for plotting                #
#           for the entire data set,  by service line level                                                          #
######################################################################################################################

# Create the function
summarize_by_service_line <- function(census_with_capacity) {
  # Define service line groups (updated to current names)
  team_groups <- list(
    "Acute Care"        = c("ACUTECARE1", "ACUTECARE2", "ACUTECARE3"),
    "Behavioral Health" = c("BehavioralHealth"),
    "Medical Ward"      = c("Team1","Team2","Team3","Team4","Team5","Team6",
                            "Team7","Team8","Team9","Team10",
                            "Team11","Team12","Team13","Team14",
                            "Team15A","HospitalTeam",
                            "MedTeam1","MedTeam2","MedTeam3","MedTeam4"),
    "Intermediate Care" = c("Team15B"),
    "Cancer Care"       = c("CancerCare1","CancerCare2","CancerCare3"),
    "Consult Service"   = c("ConsultTeamA","ConsultTeamB"),
    "Day Shift"         = c("DAY"),
    "Evening Shift"     = c("SWING"),
    "Night Shift"       = c("NIGHT")
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
  ordered_levels <- c( "Acute Care", "Behavioral Health", "Medical Ward", 
                       "Intermediate Care", "Cancer Care", "Consult Service", 
                       "Hospital Service", "Day Shift", "Evening Shift", 
                       "Night Shift" )
  
  data_serviceline_month <- data_serviceline_month |>
    dplyr::mutate(ServiceLine = factor(ServiceLine, levels = ordered_levels))
  
  # Return both daily and monthly datasets
  return(list(
    data_serviceline = data_serviceline,
    data_serviceline_month = data_serviceline_month
  ))
}