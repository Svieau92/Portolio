######################################################################################################################
# Purpose: Provide data preparation and plotting logic for team-level visualizations                                 #
######################################################################################################################

# Helper to prepare team plot data
getTeamPlotData <- function(service_line_name, team_name, census_df, look_back_period) {
  req(census_df, look_back_period)
  
  # Map look-back period to number of days
  look_back_days <- switch(look_back_period,
                           "1 Week" = 7, "2 Weeks" = 14, "1 Month" = 30,
                           "3 Months" = 90, "6 Months" = 180, "1 Year" = 365,
                           30)  # default fallback
  
  # Cutoff date based on look-back
  cutoff <- max(census_df$Date, na.rm = TRUE) - look_back_days
  
  # Filter and aggregate
  census_df |>
    dplyr::filter(ServiceLine == service_line_name, Team == team_name, Date >= cutoff) |>
    dplyr::group_by(Date) |>
    dplyr::summarise(
      Volume = sum(Volume, na.rm = TRUE),                 # total daily volume
      Capacity = sum(Capacity, na.rm = TRUE),             # total daily capacity
      utilization_rate = (Volume / Capacity) * 100,       # % utilization
      .groups = "drop"
    ) |>
    dplyr::mutate(Change = Volume - dplyr::lag(Volume))   # daily change in volume
}

# ---- Plot helpers ----

# Create Volume Line Plot
renderTeamLinePlot <- function(service_line_name, team_name, census_df, look_back_period_reactive) {
  renderPlotly({
    req(census_df(), look_back_period_reactive())
    
    # Create data set
    df <- getTeamPlotData(service_line_name, team_name, census_df(), look_back_period_reactive())
    
    # Create the plot
    p <- ggplot(df, aes(x = Date)) +
      geom_line(aes(y = Volume, text = paste0("Date: ", Date, "<br>Volume: ", Volume), group = 1),
                color = "#CFB87C", size = 0.8) +
      geom_line(aes(y = Capacity, text = paste0("Date: ", Date, "<br>Capacity: ", Capacity), group = 1),
                color = "#bcbcbc", linetype = "dashed", size = 0.6) +
      theme_minimal() +
      labs(title = team_name, subtitle = "Dashed line = Capacity", x = "Date", y = "Patients") +
      theme(axis.title.x = element_text(size = 12),
            axis.title.y = element_text(size = 12),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
            axis.text.y = element_text(size = 12),
            plot.title = element_text(size = 14, face = "bold"),
            plot.subtitle = element_text(size = 13)) +
      coord_cartesian(ylim = c(0, NA))
    
    # Convert to plotly object
    ggplotly(p, tooltip = "text") |>
      config(displayModeBar = FALSE) |>
      layout(
        hovermode = "x",
        title = list(
          text = paste0("<b>", team_name, "</b>"),
          font = list(size = 14, family = "Arial", color = "#333")
        ),
        xaxis = list(title = list(text = "Date", font = list(size = 12)), tickfont = list(size = 11)),
        yaxis = list(title = list(text = "Patients", font = list(size = 12)), tickfont = list(size = 12))
      )
  })
}

# Capacity Bar Plot
renderTeamBarPlot <- function(service_line_name, team_name, census_df, look_back_period_reactive) {
  renderPlotly({
    req(census_df(), look_back_period_reactive())
    
    # Create the data set
    df <- getTeamPlotData(service_line_name, team_name, census_df(), look_back_period_reactive())
    
    # Create the plot
    p <- ggplot(df, aes(x = Date)) +
      geom_col(aes(y = utilization_rate,
                   text = paste0("Date: ", Date, "<br>Usage: ", round(utilization_rate, 1), "%")),
               fill = "#CFB87C", width = 0.7) +
      geom_hline(yintercept = 100, color = "#bcbcbc", linetype = "dashed", size = 0.6) +
      geom_hline(yintercept = 90, color = "#bcbcbc", linetype = "dashed", size = 0.6) +
      theme_minimal() +
      labs(title = "Utilization Rates (% of Capacity)",
           subtitle = "Dashed Lines are 100% and 90% Utilization Rates",
           x = "Date", y = "Utilization Rate") +
      theme(axis.title.x = element_text(size = 12),
            axis.title.y = element_text(size = 12),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
            axis.text.y = element_text(size = 12),
            plot.title = element_text(size = 14, face = "bold"),
            plot.subtitle = element_text(size = 13)) +
      coord_cartesian(ylim = c(0, NA))
    
    # Convert to plotly object
    ggplotly(p, tooltip = "text") |>
      config(displayModeBar = FALSE) |>
      layout(
        title = list(
          text = paste0("<b>Capacity Usage (%)</b>",
                        "<br><span style='font-size:12px;'>Dashed Lines are 100% and 90% Capacities</span>"),
          font = list(size = 14, family = "Arial", color = "#333")
        ),
        xaxis = list(title = list(text = "Date", font = list(size = 12)), tickfont = list(size = 11)),
        yaxis = list(title = list(text = "% of Max Capacity", font = list(size = 12)), tickfont = list(size = 12))
      )
  })
}

# Create the Change Plot
renderTeamChangePlot <- function(service_line_name, team_name, census_df, look_back_period_reactive) {
  renderPlotly({
    req(census_df(), look_back_period_reactive())
    
    # Create the data set
    df <- getTeamPlotData(service_line_name, team_name, census_df(), look_back_period_reactive())
    
    # Create the plot
    p <- ggplot(df, aes(x = Date)) +
      geom_col(aes(y = Change, text = paste0("Date: ", Date, "<br>Change: ", Change)),
               fill = "#CFB87C") +
      labs(title = "Daily Change Plot",
           subtitle = "Values are Changes in Volume from the Previous Day",
           x = "Date", y = "Change") +
      theme_minimal() +
      theme(axis.title.x = element_text(size = 12),
            axis.title.y = element_text(size = 12),
            axis.text.x = element_text(size = 11),
            axis.text.y = element_text(size = 12),
            strip.text = element_text(size = 14),
            plot.title = element_text(size = 14, face = "bold"),
            plot.subtitle = element_text(size = 13))
    
    # Convert to plotly object
    ggplotly(p, tooltip = "text") |>
      config(displayModeBar = FALSE) |>
      layout(
        title = list(
          text = paste0("<b>Daily Change Plot</b>",
                        "<br><span style='font-size:12px;'>Values are Changes in Volume from the Previous Day</span>"),
          font = list(size = 14, family = "Arial", color = "#333")
        ),
        xaxis = list(title = list(text = "Date", font = list(size = 12)), tickfont = list(size = 11)),
        yaxis = list(title = list(text = "Patients", font = list(size = 12)), tickfont = list(size = 12))
      )
  })
}
# SPC X Chart (Individuals chart)
renderTeamXChart <- function(service_line_name, team_name, census_df, look_back_period_reactive) {
  renderPlotly({
    req(census_df(), look_back_period_reactive())
    df <- getTeamPlotData(service_line_name, team_name, census_df(), look_back_period_reactive())
    
    # Create Individuals (X) chart using qic
    p <- qic(Volume, x = Date, data = df, chart = "i", ylab = "Volume", part = "shift")
    
    # Convert to plotly object
    ggplotly(p, tooltip = c("x", "y")) |>
      style(hoverinfo = "skip", traces = 2:100) |>  # suppress extra layers
      config(displayModeBar = FALSE) |>
      layout(
        hovermode = "x",
        title = list(
          text = "<b>X Chart</b>",
          font = list(size = 14, family = "Arial", color = "#333")
        ),
        margin = list(t = 70),
        xaxis = list(title = list(text = "Date", font = list(size = 12)), tickfont = list(size = 11)),
        yaxis = list(title = list(text = "Volume", font = list(size = 12)), tickfont = list(size = 12))
      )
  })
}

# SPC MR Chart (Moving Range chart)
renderTeamMRChart <- function(service_line_name, team_name, census_df, look_back_period_reactive) {
  renderPlotly({
    req(census_df(), look_back_period_reactive())
    df <- getTeamPlotData(service_line_name, team_name, census_df(), look_back_period_reactive())
    
    # Create Moving Range (mR) chart using qic
    p <- qic(Volume, x = Date, data = df, chart = "mr", ylab = "Moving Range", part = "shift")
    
    # Convert to plotly object
    ggplotly(p, tooltip = c("x", "y")) |>
      style(hoverinfo = "skip", traces = 2:100) |>  # suppress extra layers
      config(displayModeBar = FALSE) |>
      layout(
        hovermode = "x",
        title = list(
          text = "<b>Moving Range Chart</b>",
          font = list(size = 14, family = "Arial", color = "#333")
        ),
        margin = list(t = 70),
        xaxis = list(title = list(text = "Date", font = list(size = 12)), tickfont = list(size = 11)),
        yaxis = list(title = list(text = "Moving Range", font = list(size = 12)), tickfont = list(size = 12))
      )
  })
}