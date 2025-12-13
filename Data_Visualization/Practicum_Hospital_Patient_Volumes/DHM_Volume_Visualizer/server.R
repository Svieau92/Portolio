#######################################################################################################################
# App Name: Division of Hospital Medicine Patient Volume Visualizer                                                   #
# Author: Sean Vieau                                                                                                  #
# Date Created: 9/29/2025                                                                                             #
# Last Updated:                                                                                                       #
#                                                                                                                     #
# Info: The purpose of this app is to aid the CU DHM in visualizing patient volume and capacity.                      #
#       Users are able to see in real time patient volumes and capacity on the service line and team levels,          #
#       and facet by Academic Year.                                                                                   #
#                                                                                                                     #
#       This is the server.R file, where we define the underlying logic to create all the plots                       #
#######################################################################################################################

# Define server logic 
function(input, output, session) {
  
  #######################################################################################################################
  #                             Load Helper Scripts for Data Prep, Metrics, and Plotting                                #                        #
  #######################################################################################################################

  # To keep server.R clean and maintainable, core logic is split into separate R files
  # Each script defines functions for a specific responsibility (data wrangling, metrics, plots)
  # By sourcing them here, those functions are available throughout Server.R
  source("R//summarize_monthly_data.R")
  source("R//summarize_by_service_line.R")
  source("R//summarize_by_team_level.R")
  source("R//build_custom_plot_data.R")
  source("R/service_line_metrics.R")
  source("R/service_line_plots.R")
  source("R/team_metrics.R")
  source("R/team_plots.R")
  
#######################################################################################################################
#                                        Compile User Selected Data Sets                                              #
#######################################################################################################################

  # Initialize empty data sets
  census_with_capacity <- reactiveVal(NULL)
  combined_data <- reactiveVal(NULL)
  data_daily_caps <- reactiveVal(NULL)
  
  # Read census data based on user upload
  observeEvent(input$process, {
    
    # Require user selected data sets before running
    req(input$file, input$caps)
    
    # Show spinner immediately when button is clicked
    # This gives users instant feedback that the program is "thinking"
    show_modal_spinner(spin = "fading-circle", color = "#CFB87C")
    
    # Run the function in global.R to extract and manipulate selected .xlsx files into final data set
    result <- process_uploaded_file(input$file$datapath, input$caps$datapath)
    
    # Save to universal data sets
    census_with_capacity(result$census_with_capacity)
    combined_data(result$combined_data)
    data_daily_caps(result$data_daily_caps)
    
    # Remove spinner once finished
    remove_modal_spinner()
    
    # Update a message output so users see confirmation
    output$process_message <- renderText({
      "✅ Data finished processing!"
    })
  })
  
  # Once finished, update the message
  output$process_message <- renderText({
    req(census_with_capacity())
    "✅ Data finished processing!"
  })
  
  #==== Examine Data Sets for Debugging ====#
  
  # Check the header (census_with_capacity)
  output$data_preview <- renderDT({
    census_with_capacity()
  }, options = list(pageLength = 20))  # show 20 rows per page

  # Check the header (combined_data) 
  output$data_preview2 <- renderPrint({
    req(combined_data())
    head(combined_data(), 10)
  })
  
  # Check the header (data_daily_caps)
  output$data_preview3 <- renderPrint({
    req(data_daily_caps())
    head(data_daily_caps(), 10)
  })
    
#######################################################################################################################
#                                                      Dashboard                                                      #
#######################################################################################################################
  
  #==============================================================================================#
  #                                          UI Elements                                         #                    
  #==============================================================================================#
  
  # Observe reset button
  observeEvent(input$reset_dash,
               {
                 # Reset look back period
                 updatePickerInput(session, "look_back_period",
                                   selected = "1 Month")
               })
  
  #==============================================================================================#
  #                                     Create Dashboard Data Set                                #                    
  #==============================================================================================#
  
  # Filter data set to user-selected look back period
  filtered_total_data <- reactive({
    req(census_with_capacity(), input$look_back_period)
    
    # Translate the chosen look-back option into a numeric day count
    # (e.g. "1 Week" -> 7 days, "3 Months" -> 90 days, etc.)
    look_back_days <- switch(input$look_back_period,
                             "1 Week"   = 7,
                             "2 Weeks"  = 14,
                             "1 Month"  = 30,
                             "3 Months" = 90,
                             "6 Months" = 180,
                             "1 Year"   = 365,
                             30
    )
    
    # Only look at dates within the look back period
    cutoff <- max(census_with_capacity()$Date) - look_back_days
    
    # Filter down to IMTOTAL and AVAILCAP rows
    census_with_capacity() |>
      filter(Date >= cutoff, Team %in% c("IMTOTAL", "AVAILCAP")) |>
      select(Date, Team, Volume)
  })
  
  # Pivot data to calculate the true capacities for each day (IMTOTAL + AVAILCAP)
  total_plot_data <- reactive({
    filtered_total_data() |>
      pivot_wider(names_from = Team, values_from = Volume) |>
      rename(Volume = IMTOTAL) |> 
      mutate(
        Capacity = Volume + AVAILCAP,
        utilization_rate = (Volume / Capacity)*100,
        Change = Volume - lag(Volume)
      ) 
  })
  
  #==== Examine Data Sets for Debugging ====#
  
  # Check the header (total_plot_data)
  output$data_preview12 <- renderPrint({
    req(total_plot_data())
    head(total_plot_data(), 10)
  })
  
  #==============================================================================================#
  #                                          Global Metrics                                      #                    
  #==============================================================================================#
  
  # Get change in utilization rate from current look pack period vs the previous one 
  previous_plot_data <- reactive({
    req(census_with_capacity(), input$look_back_period)
    
    # Translate the chosen look-back option into a numeric day count
    # (e.g. "1 Week" -> 7 days, "3 Months" -> 90 days, etc.)
    look_back_days <- switch(input$look_back_period,
                             "1 Week" = 7, 
                             "2 Weeks" = 14, 
                             "1 Month" = 30,
                             "3 Months" = 90, 
                             "6 Months" = 180, 
                             "1 Year" = 365, 
                             30)
    
    # Find the most recent date in the dataset
    max_date <- max(census_with_capacity()$Date, na.rm = TRUE)
    
    # Filter to the window immediately *before* the current look-back period
    # Example: if look-back is 30 days, this grabs days 31–60 before max_date
    census_with_capacity() |>
      filter(Date > (max_date - 2 * look_back_days),
             Date <= (max_date - look_back_days),
             Capacity > 0)
  })
  
  # Output the text showing the change in average % capacity between current and previous windows
  output$avg_perc_cap_delta <- renderText({
    req(total_plot_data(), previous_plot_data())
    
    # Calculate current utilization (% of capacity) for the active look-back window
    current <- (sum(total_plot_data()$Volume, na.rm = TRUE) /
                  sum(total_plot_data()$Capacity, na.rm = TRUE)) * 100
    
    # Calculate previous utilization (% of capacity) for the prior look-back window
    previous <- (sum(previous_plot_data()$Volume, na.rm = TRUE) /
                   sum(previous_plot_data()$Capacity, na.rm = TRUE)) * 100
    
    # Compute the delta (difference between current and previous utilization)
    delta <- current - previous
    
    # Format as text and round (↑ if increase, ↓ if decrease)
    paste0(round(delta, 1), "% ", ifelse(delta > 0, "↑", "↓"))
  })
  
  # Get # Service lines above 90% average capacity
  output$above_90_count <- renderText({
    req(census_with_capacity(), input$look_back_period)
    
    # Convert the selected look-back option into a numeric day count
    # (e.g. "1 Week" -> 7 days, "3 Months" -> 90 days, etc)
    look_back_days <- switch(input$look_back_period,
                             "1 Week" = 7, "2 Weeks" = 14, "1 Month" = 30,
                             "3 Months" = 90, "6 Months" = 180, "1 Year" = 365, 30)
    
    # Define cutoff date: start of the current look-back window
    cutoff <- max(census_with_capacity()$Date, na.rm = TRUE) - look_back_days
    
    # Filter census data to the current look-back window
    # Exclude rows with zero capacity to avoid divide-by-zero errors
    census_with_capacity() |>
      filter(Date >= cutoff, Capacity > 0) |>  # exclude zero-capacity rows
      group_by(ServiceLine) |> # Group by service line to calculate average utilization per line
      summarise(
        avg_utilization = mean(Volume / Capacity, na.rm = TRUE),
        .groups = "drop"
      ) |>
      filter(avg_utilization > 0.9) |> # Keep only service lines with >90% average utilization
      nrow()
  })
  
  # Get # Teams above 90% average capacity
  output$above_90_count_teams <- renderText({
    req(census_with_capacity(), input$look_back_period)
    
    # Translate the selected look-back option into a numeric day count
    # (e.g. "1 Week" -> 7 days, "3 Months" -> 90 days, etc.)
    look_back_days <- switch(input$look_back_period,
                             "1 Week" = 7, "2 Weeks" = 14, "1 Month" = 30,
                             "3 Months" = 90, "6 Months" = 180, "1 Year" = 365, 30)
    
    # Define cutoff date: start of the current look-back window
    cutoff <- max(census_with_capacity()$Date, na.rm = TRUE) - look_back_days
    
    # Filter census data to the current look-back window
    # Exclude rows with zero capacity to avoid divide-by-zero errors
    census_with_capacity() |>
      filter(Date >= cutoff, Capacity > 0) |>  # exclude zero-capacity rows
      group_by(Team) |>
      summarise(
        avg_utilization = mean(Volume / Capacity, na.rm = TRUE),
        .groups = "drop"
      ) |>
      filter(avg_utilization > 0.9) |>
      nrow()
  })
  
  #==============================================================================================#
  #                                    Inpatient Medicine Total                                  #                    
  #==============================================================================================#
  
  #===========================================================================#
  #                           Create Reactive Metrics                         #                    
  #===========================================================================#
  
  #========================== Avg Volume ==============================#
  
  # Calculate the daily average volume from the reactive data set
  avg_vol_total <- reactive({
    mean(total_plot_data()$Volume, na.rm = TRUE)
  })
  
  # Render to UI
  output$avg_vol_total <- renderText({
    paste(round(avg_vol_total(), 1))
  })
  
  #========================== Avg Available ===========================#
  
  # Calculate the daily average available from the reactive data set
  avg_avail_total <- reactive({
    mean(total_plot_data()$AVAILCAP, na.rm = TRUE)
  })
  
  # Render to UI
  output$avg_avail_total <- renderText({
    paste(round(avg_avail_total(), 1))
  })
  
  #========================== Avg % Capacity ==========================#
  
  # Calculate the daily average % Capacity from the reactive data set
  avg_perc_cap_total <- reactive({
    (sum(total_plot_data()$Volume)/sum(total_plot_data()$Capacity)) * 100
  })
  
  # Render to UI
  output$avg_perc_cap_total <- renderText({
    paste(round(avg_perc_cap_total(), 1),"%")
  })
  
  #========================== Days > 90% ==============================#
  
  # Calculate number of days >90% capacity from the reactive data set
  n_days_90_total <- reactive({
    sum(total_plot_data()$utilization_rate > 90, na.rm = TRUE)
  })
  
  # Render to UI
  output$n_days_90_total <- renderText({
    paste(round(n_days_90_total(), 1))
  })
  
  #========================== Consecutive Days > 90% ==================#
  
  # Calculate max number of consecutive days >90% capacity from the reactive data set
  n_consec_90_total <- reactive({
    rle(total_plot_data()$utilization_rate > 90) |> 
      with(max(lengths[values], na.rm = TRUE))
  })
  
  # Render to UI
  output$n_consec_90_total <- renderText({
    paste(round(n_consec_90_total(), 1))
  })
  
  #========================== Days at 100% ============================#
  
  # Calculate days at 100% capacity from the reactive data set
  n_days_100_total <- reactive({
    streaks <- rle(total_plot_data()$utilization_rate == 100)
    if (any(streaks$values, na.rm = TRUE)) {
      max(streaks$lengths[streaks$values], na.rm = TRUE)
    } else {
      0
    }
  })
  
  # Render to UI
  output$n_days_100_total <- renderText({
    paste(round(n_days_100_total(), 1))
  })
  
  #===========================================================================#
  #                   Create Inpatient Medicine Total Plots                   #                    
  #===========================================================================#
  
  # Create volume plot
  output$total_plot_vol <- renderPlotly({
    req(total_plot_data())
    
    # Create the plot
    p <- ggplot(total_plot_data(), aes(x = Date)) +
      geom_line(
        aes(y = Volume, text = paste0("Date: ", Date, "<br>Volume: ", Volume), group = 1),
        color = "#CFB87C", size = 0.8
      ) +
      geom_line(
        aes(y = Capacity, text = paste0("Date: ", Date, "<br>Capacity: ", Capacity), group = 1),
        color = "#bcbcbc", linetype = "dashed", size = 0.7
      ) +
      theme_minimal() +
      labs(x = "Date", y = "Patients") +
      theme(
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 12)
      ) +
      coord_cartesian(ylim = c(0, NA))
    
    # Convert to plotly object
    ggplotly(p, tooltip = "text") |>
      config(displayModeBar = FALSE) |>
      layout(
        hovermode = "x",
        title = list(
          text = paste0(
            "<b>Inpatient Medicine Total</b>",
            "<br><span style='font-size:12px;'>Dashed line = Capacity | Look-Back: ", input$look_back_period, "</span>"
          ),
          font = list(size = 14, family = "Arial", color = "#333"),
          x = 0.15,        
          xanchor = "left"  
          
        ),
        xaxis = list(title = list(text = "Date", font = list(size = 12)), tickfont = list(size = 11)),
        yaxis = list(title = list(text = "Patients", font = list(size = 12)), tickfont = list(size = 12))
      )
  })
  
  # Create bar plot
  output$total_plot_bar <- renderPlotly({
    req(total_plot_data())
    
    # Create the plot
    p <- ggplot(req(total_plot_data()), aes(x = Date, y = utilization_rate)) +
      geom_col(aes(text = paste0("Date: ", Date, "<br>Usage: ", round(utilization_rate, 1), "%")),
               position = "dodge", fill = "#CFB87C") +
      geom_hline(yintercept = 100, color = "#bcbcbc", linetype = "dashed", size = 0.7) +
      geom_hline(yintercept = 90, color = "#bcbcbc", linetype = "dashed", size = 0.7) +
      theme_minimal() +
      labs(
        title = "Capacity Usage (%)",
        subtitle = "Dashed Lines are 100% and 90% Utilization Rates",
        x = "Date",
        y = "% of Patient Capacity"
      ) +
      theme(
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 12),
        strip.text = element_text(size = 14),
        plot.title = element_text(size = 14, face = "bold")
      ) 
    
    # Convert to Plotly Object
    ggplotly(p, tooltip = "text") |>
      config(displayModeBar = FALSE) |>
      layout(
        title = list(
          text = paste0(
            "<b>Capacity Usage (%)</b>",
            "<br><span style='font-size:12px;'>Dashed Lines are 100% and 90% Capacities</span>"
          ),
          font = list(size = 14, family = "Arial", color = "#333")
        ),
        xaxis = list(title = list(text = "Date", font = list(size = 12)), tickfont = list(size = 11)),
        yaxis = list(title = list(text = "% of Max Capacity", font = list(size = 12)), tickfont = list(size = 12))
      )
  })
  
  # Create daily change plot
  output$total_plot_change <- renderPlotly({
    req(total_plot_data())
    
    # Create the plot
    p <- ggplot(req(total_plot_data()), aes(x = Date, y = Change)) +
      geom_col(fill = "#CFB87C") +
      labs(
        title = "Daily Change Plot",
        subtitle = "Values are Changes in Volume from the Previous Day",
        x = "Date",
        Y = "Change"
      ) +
      theme_minimal() +
      theme(
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 12),
        strip.text = element_text(size = 14),
        plot.title = element_text(size = 14, face = "bold")
      )
    
    # Convert to Plotly Object
    ggplotly(p) |>
      config(displayModeBar = FALSE) |>
      layout(
        title = list(
          text = paste0(
            "<b>Daily Change Plot</b>",
            "<br><span style='font-size:12px;'>Values are Changes in Volume from the Previous Day</span>"
          ),
          font = list(size = 14, family = "Arial", color = "#333")
        ),
        xaxis = list(title = list(text = "Date", font = list(size = 12)), tickfont = list(size = 11)),
        yaxis = list(title = list(text = "Patients", font = list(size = 12)), tickfont = list(size = 12))
      )
  })
  
  # Create xbar SPC plot
  output$x_chart_total <- renderPlotly({
    req(total_plot_data())
    
    # Create the SPC plot
    p <- qic(
      Volume, x = Date,
      data = total_plot_data(),
      chart = "i",
      ylab = "Volume",
      part = "shift"
    )
    
    # Final layout
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
  
  # mR Chart (moving range between days)
  output$mr_chart_total <- renderPlotly({
  df <- total_plot_data()

  # Create the plot
  p <- qic(
    Volume, x = Date,
    data = df,
    chart = "mr",
    title = "Moving Range Chart",
    ylab = "Moving Range",
    part = "shift"
  )

  # Convert to plotly and filter tooltips
  ggplotly(p, tooltip = c("x", "y")) |>
    style(hoverinfo = "skip", traces = 2:100) |>  # suppress extra layers
    config(displayModeBar = FALSE) |>
    layout(
      hovermode = "x",
      title = list(
        text = "<b>Moving Range Chart</b>",
        font = list(size = 14, family = "Arial", color = "#333"),
        x = 0.15,
        xanchor = "left"
      ),
      margin = list(t = 70),
      xaxis = list(title = list(text = "Date", font = list(size = 12)), tickfont = list(size = 11)),
      yaxis = list(title = list(text = "Moving Range", font = list(size = 12)), tickfont = list(size = 12))
    )
})

  #==============================================================================================#
  #                                       Service Lines                                          #                    
  #==============================================================================================#
  
  #===========================================================================#
  #                           Service Line Cards                              #                    
  #===========================================================================#
  
  # Reactive state to track which service line card is currently expanded (NULL = none)
  expanded <- reactiveVal(NULL)
  
  # ⚠️ HARD-CODED SERVICE LINES ⚠️
  # This is the master list of service lines used throughout the app
  # If new service lines are added or existing ones renamed, UPDATE THIS VECTOR
  service_lines <- c(
    "Addiction Medicine", "ACE", "General Medicine", "HMS7",
    "Stepdown", "Med Oncology", "Medicine Consults", "Transplant",
    "Day", "Night", "Swing"
  )
  
  #===================== Expansion / Collapse Logic ===================#
  
  # This section controls how service line cards expand/collapse in the UI.
  # - Each card has its own "expand" button
  # - A global "collapse all" trigger resets everything back to the default state.
  # - The expanded() reactiveVal tracks which card is currently open.
  # - teamViewState keeps track of whether team-level cards are visible inside each service line
  
  # Dynamically create expand triggers for all service line cards
  lapply(seq_along(service_lines), function(i) {
    # For each service line index, listen for its card button being clicked
    observeEvent(input[[paste0("card", i)]], {
      # When clicked, set expanded() to that card’s ID (e.g. "card1", "card2", etc.)
      # This tells the UI which card should be shown in expanded mode
      expanded(paste0("card", i))
    })
  })
  
  # Collapse trigger: handles the "Collapse All" trigger
  observeEvent(input$collapse_all, {
    # Reset expanded() to NULL → no card is expanded
    expanded(NULL)
    
    # Reset teamViewState for every service line back to FALSE
    # This hides any team-level cards were opened when 'collapse all' is clicked
    lapply(service_lines, function(sl) {
      teamViewState[[sl]] <- FALSE
    })
  })
  
  #===========================================================================#
  #                        Create Service Line Plots                          #                    
  #===========================================================================#
    
  # Apply function to all service lines to create volume line plots for collapsed vs expanded cards
  lapply(service_lines, function(sl) {
    # Clean ID base
    id_base <- gsub(" ", "_", sl)
    
    # Collapsed plot
    collapsed_id <- paste0("vol_line_plot_collapsed_", id_base)
    output[[collapsed_id]] <- renderLinePlot(sl, census_with_capacity, reactive({ input$look_back_period }))
    
    # Expanded plot
    expanded_id <- paste0("vol_line_plot_expanded_", id_base)
    output[[expanded_id]] <- renderLinePlot(sl, census_with_capacity, reactive({ input$look_back_period }))
    
  })
  
  # Register bar plots for each service line
  lapply(service_lines, function(sl) {
    output[[paste0("bar_plot_", gsub(" ", "_", sl))]] <- 
      renderBarPlot(sl, census_with_capacity, reactive({ input$look_back_period }))
  })
  
  # Register change plots for each service line
  lapply(service_lines, function(sl) {
    output[[paste0("change_plot_", gsub(" ", "_", sl))]] <- 
      renderChangePlot(sl, census_with_capacity, reactive({ input$look_back_period }))
  })
  
  # Register SPC plots for each service line
  lapply(service_lines, function(sl) {
    output_id_x  <- paste0("x_chart_", gsub(" ", "_", sl))
    output_id_mr <- paste0("mr_chart_", gsub(" ", "_", sl))
    
    output[[output_id_x]]  <- renderXChart(sl, census_with_capacity, reactive({ input$look_back_period }))
    output[[output_id_mr]] <- renderMRChart(sl, census_with_capacity, reactive({ input$look_back_period }))
  })
  
  #==============================================================================================#
  #                                            Teams                                             #                    
  #==============================================================================================#
  
  #===========================================================================#
  #                          Team Card Logic                                  #
  #===========================================================================#
  
  # Reactive state for tracking which teams are visible
  teamViewState <- reactiveValues()
  
  # Helper function: get teams for a given service line
  teams_in_service_line <- function(service_line_name) {
    req(census_with_capacity())
    census_with_capacity() |>
      filter(ServiceLine == service_line_name) |>
      distinct(Team) |>
      pull(Team)
  }
  
  # Define helper function for mini metric cards for each service line
  metric_tiles <- function(metrics, expanded = FALSE) {
    labels <- c(
      "Avg Daily Volume", "Avg Available", "Avg % Capacity",
      "Days > 90%", "Consecutive Days > 90%", "Days at 100%"
    )
    
    values <- c(
      metrics$avg_volume,
      metrics$avg_capacity,
      paste0(metrics$avg_pct, "%"),
      metrics$days_over_90,
      metrics$longest_streak,
      metrics$days_at_100
    )
    
    tiles <- lapply(seq_along(labels), function(i) {
      column(if (expanded) 2 else 4, div(
        style = "background-color: #f8f9fa; border-radius: 6px; padding: 0.4em; height: 60px; display: flex; flex-direction: column; justify-content: center; align-items: center;",
        tags$div(labels[i], style = "font-size: 0.70em; color: #6c757d;"),
        tags$div(values[i], style = "font-size: 0.9em; font-weight: bold;")
      ))
    })
    
    if (expanded) {
      fluidRow(tiles)
    } else {
      tagList(
        fluidRow(tiles[1:3]),
        fluidRow(tiles[4:6])
      )
    }
  }
  
  #===================== Expansion / Collapse Logic ===================#
  
  # This section controls how team cards are shown/hidden inside each service line card
  # - "Inspect Teams" button expands card and shows all plots and metrics for all teams comprising that service line
  # - "Hide Teams" button collapses the team cards and clears plots/messages
  # - teamViewState tracks whether teams are currently visible for each service line
  # - Outputs for team plots are dynamically created/destroyed based on user actions
  
  # Logic to handle creation of Team cards when Inspect Teams button is clicked
  lapply(service_lines, function(service_line) {
    observeEvent(input[[paste0("inspect_", gsub(" ", "_", service_line))]], {
      
      # Mark this service line’s team view as active
      teamViewState[[service_line]] <- TRUE
      
      # Get the list of teams for this service line
      team_list  <- teams_in_service_line(service_line)
      message_id <- paste0("team_message_", gsub(" ", "_", service_line))
      
      if (length(team_list) > 1) {
        
        # Clear any previous "only one team" message
        output[[message_id]] <- renderUI({ NULL })
        
        # For each team, dynamically create outputs for all plots
        lapply(team_list, function(team) {
          team_id <- gsub(" ", "_", team)  # safe ID for output names
          
          # Volume Line Plots
          output[[paste0("team_line_plot_", team_id)]] <-
            renderTeamLinePlot(service_line, team, census_with_capacity,
                               reactive({ input$look_back_period }))
          
          # Capacity Bar Plots
          output[[paste0("team_bar_plot_", team_id)]] <-
            renderTeamBarPlot(service_line, team, census_with_capacity,
                              reactive({ input$look_back_period }))
          
          # Change plot 
          output[[paste0("team_change_plot_", team_id)]] <-
            renderTeamChangePlot(service_line, team, census_with_capacity,
                                 reactive({ input$look_back_period }))
          
          # X Chart 
          output[[paste0("team_x_plot_", team_id)]] <-
            renderTeamXChart(service_line, team, census_with_capacity,
                             reactive({ input$look_back_period }))
          
          # mR Chart 
          output[[paste0("team_mr_plot_", team_id)]] <-
            renderTeamMRChart(service_line, team, census_with_capacity,
                              reactive({ input$look_back_period }))
        })
      } else {
        
        # If only one team exists, show a message instead of plots
        output[[message_id]] <- renderUI({
          div(
            style = "color: #CFB87C; font-weight: bold; padding: 10px;",
            "This service line only contains one team - no breakdown available."
          )
        })
      }
    })
  })
  
  # Logic to hide team cards when Hide Teams button is clicked
  lapply(service_lines, function(service_line) {
    observeEvent(input[[paste0("hide_teams_", gsub(" ", "_", service_line))]], {
      
      # Mark this service line’s team view as inactive
      teamViewState[[service_line]] <- FALSE
      
      # Clear team-level plots for all teams in this service line
      team_list <- teams_in_service_line(service_line)
      lapply(team_list, function(team) {
        team_id <- gsub(" ", "_", team)
        output[[paste0("team_line_plot_", team_id)]]   <- NULL
        output[[paste0("team_bar_plot_", team_id)]]    <- NULL
        output[[paste0("team_change_plot_", team_id)]] <- NULL
        output[[paste0("team_x_plot_", team_id)]]      <- NULL   
        output[[paste0("team_mr_plot_", team_id)]]     <- NULL
      })
      
      # Clear any "only one team" message
      output[[paste0("team_message_", gsub(" ", "_", service_line))]] <- renderUI({ NULL })
    })
  })
  
  #===========================================================================#
  #                            Create Team Plots                              #                    
  #===========================================================================#
  
  # This function builds the UI for a single team card inside a service line
  # - Called by card_layout when a service line is expanded and "Inspect Teams" is active
  # - Output IDs are dynamically generated based on the team name (spaces replaced with underscores)
  # - All plots are rendered reactively elsewhere in server logic when Inspect/Hide Teams are triggered
  
  # Create mini card helper function
  teamMiniCardUI <- function(service_line_name, team_name) {
    # Ensure census data is available before building the card
    req(census_with_capacity())
    
    # Create the card for this team
    div(class = "team-mini-card",
        card(
          style = "margin-bottom: 10px;",
          
          # Card header: team name with custom styling
          card_header(
            team_name,
            class = "bg-white border border-primary",
            style = "color: #CFB87C; font-weight: bold;"
          ),
          
          # Card body: Fill with metrics and plots for this team
          card_body(
            
            # Metric tiles: summary stats for this team
            metric_tiles(
              getTeamMetrics(service_line_name, team_name,
                             census_with_capacity(), input$look_back_period),
              expanded = FALSE
            ),
            
            # Plots for this team (IDs are dynamically generated)
            div(style = "margin-bottom: 1px;", plotlyOutput(paste0("team_line_plot_", gsub(" ", "_", team_name)), height = "160px")),
            div(style = "margin-bottom: 1px;", plotlyOutput(paste0("team_bar_plot_", gsub(" ", "_", team_name)), height = "160px")),
            div(style = "margin-bottom: 1px;", plotlyOutput(paste0("team_change_plot_", gsub(" ", "_", team_name)), height = "160px")),
            div(style = "margin-bottom: 1px;", plotlyOutput(paste0("team_x_plot_", gsub(" ", "_", team_name)), height = "180px")),
            div(style = "margin-bottom: 1px;", plotlyOutput(paste0("team_mr_plot_", gsub(" ", "_", team_name)), height = "180px"))
          )
        )
    )
  }
  
  #===========================================================================#
  #                  Final Service Line + Team Card Deployment                #
  #===========================================================================#

  # Create logic to render expandable cards for each service line and fill in plots
  output$card_layout <- renderUI({
    
    # Track which card is currently expanded (NULL = none)
    exp <- expanded()
    
    # Build card UI for each service line (collapsed or expanded state handled dynamically)
    fluidRow(
      lapply(seq_along(service_lines), function(i) {
        id <- paste0("card", i)                       # Unique ID for each card
        is_expanded <- isTRUE(exp == id)              # TRUE if this card is expanded
        col_width <- if (is_expanded) 12 else 3       # Expanded cards span full width, collapsed are narrow
        service_line <- service_lines[i]              # Current service line name
        
        # Call metrics inside reactive context
        metrics <- getServiceLineMetrics(service_line, census_with_capacity(), input$look_back_period)
        
        # Create card
        column(col_width,
               card(
                 style = if (is_expanded) "height: auto; position: relative;" else "height: 460px; position: relative;",
                 
                 # Create title of service line for each card
                 card_header(service_line, class = "bg-primary"),
                 
                 # Card body: handle the expand/collapse button + fill metrics and plots
                 card_body(
                   div(
                     style = "position: absolute; top: 5px; right: 10px; z-index: 10;",
                     
                     # Show Collapse All button when this card is expanded
                     if (is_expanded) {
                       actionButton("collapse_all", NULL,
                                    icon = icon("compress", verify_fa = FALSE),
                                    class = "btn btn-sm btn-outline-dark")
                     }
                     # Show Expand button when this card is collapsed
                     else {
                       actionButton(id, NULL,
                                    icon = icon("expand", verify_fa = FALSE),
                                    class = "btn btn-sm btn-outline-light")
                     }
                   ),
                   
                   # Inject metrics here
                   metric_tiles(metrics, expanded = is_expanded),
                   
                   # Collapsed card content
                   if(!is_expanded) {
                     
                     # Only show Volume Line Plot for the Service Line
                     plotlyOutput(paste0("vol_line_plot_collapsed_", gsub(" ", "_", service_line)), height = "auto")
                   },
                   
                   # Expanded card content
                   if (is_expanded) {
                    
                     # Show all plots for the selected Service Line
                     tagList(
                       
                       # First row: volume, bar, change plots
                       fluidRow(
                         
                         # Volume line plot
                         column(width = 4, plotlyOutput(paste0("vol_line_plot_expanded_", gsub(" ", "_", service_line)), height = "280px")),
                         
                         # Capacity Bar plot
                         column(width = 4, plotlyOutput(paste0("bar_plot_", gsub(" ", "_", service_line)), height = "280px")),
                         
                         # Change plot
                         column(width = 4, plotlyOutput(paste0("change_plot_", gsub(" ", "_", service_line)), height = "280px"))
                       ),
                       
                       # Second row: SPC plots (X chart + mR chart)
                       fluidRow(
                         
                         # SPC X Chart
                         column(width = 6, plotlyOutput(paste0("x_chart_", gsub(" ", "_", service_line)), height = "260px")),
                         
                         # SPC mR Chart
                         column(width = 6, plotlyOutput(paste0("mr_chart_", gsub(" ", "_", service_line)), height = "260px"))
                       ),
                       
                       # Logic for "Inspect Teams" and "Hide Teams" buttons
                       fluidRow(
                         column(width = 12,
                                
                                # Button to show all team cards for this service line
                                actionButton(
                                  paste0("inspect_", gsub(" ", "_", service_line)),   # dynamic ID based on service line
                                  "Inspect Teams",                                    # button label
                                  class = "btn btn-outline-primary btn-sm",           # styling: small, outlined, primary
                                  style = "float: right; margin-bottom: 10px;"        # position: right side of card
                                ),
                                
                                # Button to hide all team cards for this service line
                                actionButton(
                                  paste0("hide_teams_", gsub(" ", "_", service_line)),# dynamic ID based on service line
                                  "Hide Teams",                                       # button label
                                  class = "btn btn-outline-primary btn-sm",           # styling: small, outlined, primary
                                  style = "float: right; margin-right: 10px; margin-bottom: 10px;" # position: right side
                                )
                         )
                       ),
                       
                       # Display "only one team" notice for this service line if applicable
                       uiOutput(paste0("team_message_", gsub(" ", "_", service_line))),
                       
                       # Render mini cards for each team inside the expanded service line card
                       fluidRow({
                         team_list <- teams_in_service_line(service_line)   # Get all teams belonging to this service line
                         
                         # If team view is active AND there is more than one team, build mini cards for each
                         if (isTRUE(teamViewState[[service_line]]) && length(team_list) > 1) {
                           lapply(team_list, function(team) {
                             column(width = 3, teamMiniCardUI(service_line, team))  # Each team card occupies 3 columns
                           })
                         } else {
                           NULL  # Otherwise, show nothing (either hidden or only one team exists)
                         }
                       })
                       
                     )
                   }
                 )
               )
        )
      })
    )
  })
  
#######################################################################################################################
#                                                 Historical Trends                                                   #
#######################################################################################################################
  #  NOTE: The Historical Trends panel, where the user can look back volume and capacity trends for  previous years,
  #         is a future section I would like to add. I will keep the skeleton of the code here for future iterations
  #         So that we can more easily implement this panel in the future.
  # 
  # #==============================================================================================#
  # #                                          UI Elements                                         #                    
  # #==============================================================================================#
  # 
  # # Create service line selector filter
  # output$service_line_selector_ui <- renderUI({
  #   req(census_with_capacity())
  #   pickerInput("service_line_selector",
  #               label = "Select Service Lines:",
  #               choices = sort(unique(census_with_capacity()$ServiceLine)),
  #               multiple = TRUE,
  #               selected = unique(census_with_capacity()$ServiceLine))
  # })
  # 
  # # Create team level filter
  # output$team_selector_ui <- renderUI({
  #   req(census_with_capacity())
  #     pickerInput("team_selector", label = "Select Teams:",
  #                 choices = unique(census_with_capacity()$Team),
  #                 multiple = TRUE,
  #                 selected = unique(census_with_capacity()$Team))
  # })
  # 
  # #==============================================================================================#
  # #                                  Create Academic Year Data Set                               #                    
  # #==============================================================================================#
  # 
  # # Initialize empty container
  # data_months <- reactiveVal(NULL)
  # 
  # # Acquire monthly averages of volumes and capacities
  # observeEvent(input$process, {
  #   req(combined_data(), data_daily_caps())
  #   
  #   # Run the function to create data set with monthly averages for volume and capacity
  #   data_months(summarize_monthly_data(combined_data(), data_daily_caps()))
  # })
  # 
  # # Check the header (data_months)
  # output$data_preview4 <- renderPrint({
  #   req(census_with_capacity())
  #   head(census_with_capacity(), 10)
  # })

  # 
  # #==============================================================================================#
  # #                                  Create Academic Year Plots                                  #                    
  # #==============================================================================================#
  # # Plot Monthly IM TOTALs
  # output$AY_vol <- renderPlot({
  #   req(data_months())
  #   ggplot(data_months(), aes(x = Month_label, group = AY, color = AY)) +
  #   geom_line(aes(y = average_IM), size = 1.2) +
  #   geom_line(aes(y = average_cap), size = 1.2, linetype = "dashed") +
  #   theme_minimal() +
  #   labs(
  #     title = "Actual Patient Volume vs Capacity",
  #     subtitle = "Dashed Lines are Total Capacities",
  #     x = NULL,
  #     y = "Average Patient Volume"
  #   ) +
  #   facet_wrap(~ AY, nrow = 2) +
  #   theme(
  #     axis.title.x = element_text(size = 14),
  #     axis.title.y = element_text(size = 14),
  #     axis.text.x = element_text(size = 11),
  #     axis.text.y = element_text(size = 12),
  #     strip.text = element_text(size = 14),
  #     plot.title = element_text(size = 18, face = "bold")
  #   )
  # })
  #  
  # # Plot Monthly utilization rates
  # output$AY_perc <- renderPlot({
  #   req(data_months())
  #   ggplot(data_months(), aes(x = Month_label, y = utilization_rate, fill = AY, color = AY,  group = AY)) +
  #   geom_col(position = "dodge") +
  #   geom_hline(yintercept = 100, color = "red", linetype = "dashed", size = 1.05) +
  #   geom_hline(yintercept = 90, color = "grey40", linetype = "dashed", size = 1.05) +
  #   theme_minimal() +
  #   labs(
  #     title = "% of Capacity",
  #     subtitle = "Dashed Lines are 100% and 90% Utilization Rates",
  #     x = "Month",
  #     y = "% of Patient Capacity"
  #   ) +
  #   theme(
  #     axis.title.x = element_text(size = 14),
  #     axis.title.y = element_text(size = 14),
  #     axis.text.x = element_text(size = 11),
  #     axis.text.y = element_text(size = 12),
  #     strip.text = element_text(size = 14),
  #     plot.title = element_text(size = 18, face = "bold")
  #   ) +
  #   facet_wrap(~ AY)
  # })
  # 
# #######################################################################################################################
# #                                               Service Line Level Plots                                              #
# #######################################################################################################################
# 
  # #==============================================================================================#
  # #                                  Create Service Line Data Set                                #
  # #==============================================================================================#
  # 
  # # Initialize empty container
  # data_serviceline <- reactiveVal(NULL)
  # data_serviceline_month <- reactiveVal(NULL)
  # 
  # # Acquire monthly averages of volumes and capacities
  # observeEvent(input$process, {
  #   req(census_with_capacity())
  # 
  #   # Run the function to create service line level data sets
  #   result <- summarize_by_service_line(census_with_capacity())
  #     data_serviceline(result$data_serviceline)
  #     data_serviceline_month(result$data_serviceline_month)
  # })
  # 
  # # Check the header
  # output$data_preview5 <- renderPrint({
  #   req(data_serviceline())
  #   head(data_serviceline(), 10)
  # })
#   
#   # Check the header
#   output$data_preview6 <- renderPrint({
#     req(data_serviceline_month())
#     head(data_serviceline_month(), 10)
#   })
# 
#   #==============================================================================================#
#   #                                     Create Service Line Plots                                #                    
#   #==============================================================================================#   
#  
#   # Create plot
#   output$service_vol <- renderPlot({
#     ggplot(data_serviceline_month(), aes(x = Month)) +
#     geom_line(aes(y = Volume, color = ServiceLine), size = 1.2) +
#     geom_line(aes(y = Capacity, color = ServiceLine), linetype = "dashed", size = 1.05) +
#     theme_minimal() +
#     facet_wrap(~ ServiceLine, scales = "free_y", ncol = 4) +
#     labs(
#       title = "Volume and Capacity by Service Line",
#       subtitle = "Dashed Lines are Capacities",
#       x = "Date",
#       y = "Patients"
#     ) +
#     theme(
#       axis.title.x = element_text(size = 14),
#       axis.title.y = element_text(size = 14),
#       axis.text.x = element_text(size = 11),
#       axis.text.y = element_text(size = 12),
#       strip.text = element_text(size = 14),
#       plot.title = element_text(size = 18, face = "bold")
#     )
#   })
# 
#   # Create percentages bar plot
#   output$service_perc <- renderPlot({
#     ggplot(data_serviceline_month(), aes(x = Month)) +
#     geom_col(aes(y = utilization_rate, fill = ServiceLine), size = 1.2) +
#     geom_hline(yintercept = 100, color = "red", linetype = "dashed", size = 1.05) +
#     geom_hline(yintercept = 80, color = "grey40", linetype = "dashed", size = 1.05) +
#     theme_minimal() +
#     facet_wrap(~ ServiceLine, scales = "free_y", ncol = 4) +
#     labs(
#       title = "Volume as a Percentage by Service Line",
#       x = "Date",
#       y = "Percentage of Capacity"
#     ) +
#     theme(
#       axis.title.x = element_text(size = 14),
#       axis.title.y = element_text(size = 14),
#       axis.text.x = element_text(size = 11),
#       axis.text.y = element_text(size = 12),
#       strip.text = element_text(size = 14),
#       plot.title = element_text(size = 18, face = "bold")
#     )
#   })
# 
# #######################################################################################################################
# #                                                   Team Level Plots                                                  #
# #######################################################################################################################
#   
#   #==============================================================================================#
#   #                                    Create Team Level Data Set                                #                    
#   #==============================================================================================#
#   
#   # Initialize empty containers
#   data_team <- reactiveVal(NULL)
#   data_team_month <- reactiveVal(NULL)
#   
#   # Generate team-level summaries after upload
#   observeEvent(input$process, {
#     req(census_with_capacity())
#     
#     # Run the function and unpack results
#     result <- summarize_by_team_level(census_with_capacity())
#       data_team(result$data_team)
#       data_team_month(result$data_team_month)
#   })
#   
#   # Preview: team-level daily data
#   output$data_preview7 <- renderPrint({
#     req(data_team())
#     head(data_team(), 10)
#   })
#   
#   # Preview: team-level monthly summary
#   output$data_preview8 <- renderPrint({
#     req(data_team_month())
#     head(data_team_month(), 10)
#   })
# 
#   #==============================================================================================#
#   #                                    Create Team Level Plots                                   #                    
#   #==============================================================================================#
#  
#    # Create volume and capacities plot
#   output$team_vol <- renderPlot({
#     ggplot(data_team_month(), aes(x = Month)) +
#     geom_line(aes(y = Volume, color = Team), size = 1.2) +
#     geom_line(aes(y = Capacity, color = Team), linetype = "dashed", size = 1.05) +
#     theme_minimal() +
#     facet_wrap(~ Team, scales = "free_y", ncol = 4) +
#     labs(
#       title = "Volume and Capacity by Team",
#       subtitle = "Dashed Lines are Capacities",
#       x = "Date",
#       y = "Patients"
#     ) +
#       theme(
#         axis.title.x = element_text(size = 14),
#         axis.title.y = element_text(size = 14),
#         axis.text.x = element_text(size = 11),
#         axis.text.y = element_text(size = 12),
#         strip.text = element_text(size = 14),
#         plot.title = element_text(size = 18, face = "bold")
#       )
#   })
# 
#   # Create percentages plot
#   output$team_perc <- renderPlot({
#     ggplot(data_team_month(), aes(x = Month)) +
#     geom_col(aes(y = utilization_rate, fill = Team), size = 1.2) +
#     geom_hline(yintercept = 100, color = "red", linetype = "dashed") +
#     geom_hline(yintercept = 80, color = "grey40", linetype = "dashed") +
#     theme_minimal() +
#     facet_wrap(~ Team, scales = "free_y", ncol = 4) +
#     labs(
#       title = "Volume as a Percentage by Team",
#       subtitle = "Dashed Lines are 100% and 80% Utilization Rates",
#       x = "Date",
#       y = "Percentage of Capacity"
#     ) +
#       theme(
#         axis.title.x = element_text(size = 14),
#         axis.title.y = element_text(size = 14),
#         axis.text.x = element_text(size = 11),
#         axis.text.y = element_text(size = 12),
#         strip.text = element_text(size = 14),
#         plot.title = element_text(size = 18, face = "bold")
#       )
#   })
# 
# #######################################################################################################################
# #                                           Statistical Process Control Charts                                        #
# #######################################################################################################################  
# 
#   #==============================================================================================#
#   #                                          UI Elements                                         #                    
#   #==============================================================================================#
#   
#   # Make the service lines accessible for SPC ui element
#   output$service_selector_ui <- renderUI({
#     req(data_serviceline())
#     selectInput(
#       inputId = "service_selector_2",
#       label = "Choose Service Line:",
#       choices = sort(unique(data_serviceline()$ServiceLine)),
#       selected = "Med Oncology"
#     )
#   })
#   
#   #==============================================================================================#
#   #                                       Create SPC Plots                                       #                    
#   #==============================================================================================#
#   
#   # Create reactive data set for selected service line level
#   spc_data <- reactive({
#     req(input$service_selector_2)
#     data_serviceline() |>
#       filter(Team == input$service_selector_2) |>
#       filter(Date >= as.Date("2025-01-17"))
#   })
#   
#   # Check the header
#   output$data_preview10 <- renderPrint({
#     req(spc_data())
#     head(spc_data(), 10)
#   })
#   
#   # Create X plot
#   output$spc_medonc_x <- renderPlot({
#     qic(
#     Volume,
#     x = Date,
#     data = spc_data(),
#     chart = "i",  # e.g., "i", "xbar"
#     title = paste0(input$service_selector_2, " - X Chart"),
#     ylab = "Volume",
#     part = "shift"  # optional: use for phase splits
#   )
#   })
# 
#   # Create mR plot
#   output$spc_medonc_mr <- renderPlot({
#     qic(
#     Volume,
#     x = Date,
#     data = spc_data(),
#     chart = "mr",  # e.g., "i", "xbar"
#     title = paste0(input$service_selector_2, " - mR Chart"),
#     ylab = "Moving Range"
#   )
#   })

#######################################################################################################################
#                                                Create Your Own Plot                                                 #
#######################################################################################################################
 
  #==============================================================================================#
  #                                          UI Elements                                         #                    
  #==============================================================================================#
  
  # Observe Reset Button
  observeEvent(input$reset_cyo,
               {
                 # Reset date range selector
                 updateDateRangeInput(session, "date_range_selector_cyo",
                                      start = "2021-01-01",
                                      end   = Sys.Date())

                 # Reset aggregation level
                 updateRadioGroupButtons(session, "aggregation_level",
                                         selected = "Months")

                 # Reset capacity on/off toggle
                 updateMaterialSwitch(session, "capacity_toggle",
                                      value = TRUE)

                 # Reset service line selector
                 updatePickerInput(session, "service_line_selector_cyo",
                                   selected = unique(census_with_capacity()$ServiceLine[1]))

                 # Reset number of custom groups
                 updateSliderInput(session, "num_custom_groups",
                                   value = 0)

               })

  # Create service line selector filter for Create-your-own-plot
  output$service_line_selector_cyo_ui <- renderUI({
    req(census_with_capacity())
    pickerInput("service_line_selector_cyo",
                label = "Select Service Lines:",
                choices = sort(unique(census_with_capacity()$ServiceLine)),
                multiple = TRUE,
                selected = unique(census_with_capacity()$ServiceLine[1]))
  })
  
  # Render UI elements for custom groups based on slider input
  # (Provide new boxes for each number of custom groups selected)
  output$custom_group_inputs <- renderUI({
    
    # Read the number of custom groups requested from the slider
    n <- input$num_custom_groups
    
    # If no groups requested (NULL or 0), return nothing
    if (is.null(n) || n == 0) return(NULL)
    
    # For each group index (1 to n), build a set of inputs:
    lapply(seq_len(n), function(i) {
      tagList(
        # Text box to name the custom group
        textInput(
          inputId = paste0("custom_group_name_", i),   # unique ID per group
          label   = paste("Custom Group", i, "Name"),  # label shown in UI
          value   = paste("Custom Group", i)           # default value
        ),
        
        # Multi-select dropdown to assign service lines to this group
        pickerInput(
          inputId = paste0("custom_group_lines_", i),  # unique ID per group
          label   = paste("Select Service Lines for Custom Group", i),
          choices = unique(census_with_capacity()$ServiceLine), # available service lines
          multiple = TRUE                              # allow selecting more than one
        )
      )
    }) |> tagList() # Wrap all groups together into one UI object
  })
  
  
  #==============================================================================================#
  #                                  Create Service Line Data Set                                #
  #==============================================================================================#

  # Initialize empty container
  data_serviceline <- reactiveVal(NULL)
  data_serviceline_month <- reactiveVal(NULL)

  # Acquire monthly averages of volumes and capacities
  observeEvent(input$process, {
    req(census_with_capacity())

    # Run the function to create service line level data sets
    result <- summarize_by_service_line(census_with_capacity())
      data_serviceline(result$data_serviceline)
      data_serviceline_month(result$data_serviceline_month)
  })
  
  #==== Examine Data Sets for Debugging ====#

  # Check the header
  output$data_preview5 <- renderPrint({
    req(data_serviceline())
    head(data_serviceline(), 10)
  })

    # Check the header
    output$data_preview6 <- renderPrint({
      req(data_serviceline_month())
      head(data_serviceline_month(), 10)
    })

  #==============================================================================================#
  #                            Create the Create-Your-Own-Plot Data Set                          #                    
  #==============================================================================================#
  
  # Create data set for create-your-own-plot section
  data_cyo_combined <- reactive({
    req(data_serviceline(),
        input$aggregation_level,
        input$service_line_selector_cyo,
        input$date_range_selector_cyo)
    
    # Account for selection of custom groups
    n <- input$num_custom_groups
    custom_group_names <- lapply(seq_len(n), function(i) input[[paste0("custom_group_name_", i)]])
    custom_group_lines <- lapply(seq_len(n), function(i) input[[paste0("custom_group_lines_", i)]])
    
    # Call the function to build data set based on selected filters
    build_custom_plot_data(
      data_serviceline = data_serviceline(),
      aggregation_level = input$aggregation_level,
      selected_lines = input$service_line_selector_cyo,
      date_range = input$date_range_selector_cyo,
      num_custom_groups = n,
      custom_group_names = custom_group_names,
      custom_group_lines = custom_group_lines
    )
  })
  
  #==== Examine Data Sets for Debugging ====#
  
  # Preview: CYOP data
  output$data_preview9 <- renderPrint({
    req(data_cyo_combined())
    head(data_cyo_combined(), 10)
  })
  
  #==============================================================================================#
  #                            Create the Create-Your-Own-Plot Plot                              #                     
  #==============================================================================================#
  
  # Create plot
  output$plot_cyo <- renderPlotly({
    req(data_cyo_combined())
    
    # Remove rows with NA in key columns to prevent ggplotly crash
    plot_data <- data_cyo_combined() |>
      dplyr::filter(!is.na(Aggregation), !is.na(Volume), !is.na(Capacity), !is.na(ServiceLine))
    
    # Early exit if no data remains
    if (nrow(plot_data) == 0) {
      return(plotly_empty())
    }
    
    # Build base ggplot object
    base_plot <- ggplot(plot_data, aes(x = Aggregation)) +
      geom_line(aes(y = Volume, color = ServiceLine), size = 0.80)
    
    # Toggle capacity lines
    if (input$capacity_toggle) {
      base_plot <- base_plot +
        geom_line(aes(y = Capacity, color = ServiceLine, group = ServiceLine), linetype = "dashed", size = .80)
    }
    
    # Continue making plot
    full_plot <- base_plot +
      theme_minimal() +
      labs(
        x = input$aggregation_level,
        y = "Patients"
      ) +
      theme(
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 15),
        strip.text = element_text(size = 17),
        plot.title = element_text(size = 19, face = "bold"),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16)
      )
    
    # Convert to plotly and adjust legend positioning
    plotly_obj <- ggplotly(full_plot) |>  
      layout(
        hovermode = "x",  # Shows all upon hover
        
        # Mimic theme_minimal() in ggplotly
        xaxis = list(
          title = input$aggregation_level,
          showgrid = TRUE,
          gridcolor = "lightgray",
          zeroline = FALSE,
          tickfont = list(size = 12),
          titlefont = list(size = 16)
        ),
        yaxis = list(
          title = "Patients",
          showgrid = TRUE,
          gridcolor = "lightgray",
          zeroline = FALSE,
          tickfont = list(size = 13),
          titlefont = list(size = 16)
        ),
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = -0.2,
          font = list(size = 12)
        ),
        font = list(size = 14),
        plot_bgcolor = "white",
        paper_bgcolor = "white"
      ) |>
      
      # Hide zoom/pan buttons
      config(displayModeBar = FALSE)
    
    # Return the plotly object
    plotly_obj
  })
  
  #==============================================================================================#
  #                                     Create the CYOP SCP Plots                                #                    
  #==============================================================================================#
  
  # Create spc data set for cyop section
  spc_data_cyo <- reactive({
    req(data_serviceline(),
        input$service_line_selector_cyo,
        input$date_range_selector_cyo)

    data_serviceline() |>
      filter(ServiceLine %in% input$service_line_selector_cyo) |>
      filter(Date >= input$date_range_selector_cyo[1],
             Date <= input$date_range_selector_cyo[2])
  })
  
  # Create Plots
  output$spc_service_cards <- renderUI({
    req(spc_data_cyo(), data_serviceline_month(), data_cyo_combined(), input$service_line_selector_cyo)
    
    # Pre-split data once per service line
    spc_split <- split(spc_data_cyo(), spc_data_cyo()$ServiceLine)
    cyo_split <- split(data_cyo_combined(), data_cyo_combined()$ServiceLine)
    
    cards <- input$service_line_selector_cyo |> lapply(function(sl) {
      df_spc <- spc_split[[sl]]
      df_cyo <- cyo_split[[sl]]
      
      if (is.null(df_spc) && is.null(df_cyo)) return(NULL)
      
      xname <- paste0("x_plot_", make.names(sl))
      mrname <- paste0("mr_plot_", make.names(sl))
      cyo_barname <- paste0("cyo_barplot_", make.names(sl))
      
      # Calculate date span once
      date_span <- as.numeric(difftime(max(df_cyo$Aggregation), min(df_cyo$Aggregation), units = "days"))
      breaks <- case_when(
        date_span <= 30     ~ "1 day",
        date_span <= 90     ~ "1 week",
        date_span <= 365    ~ "1 month",
        date_span <= 1095   ~ "3 months",
        TRUE                ~ "6 months"
      )
      
      # Render X Chart
      output[[xname]] <- renderPlot({
        qic(Volume, x = Date, data = df_spc, chart = "i",
            title = paste0(sl, " - X Chart"), ylab = "Volume", part = "shift")
      })
      
      # Render mR Chart
      output[[mrname]] <- renderPlot({
        qic(Volume, x = Date, data = df_spc, chart = "mr",
            title = paste0(sl, " - mR Chart"), ylab = "Moving Range", part = "shift")
      })
      
      # Render CYOP Utilization Bar Plot
      output[[cyo_barname]] <- renderPlot({
        ggplot(df_cyo, aes(x = Aggregation)) +
          geom_col(aes(y = utilization_rate, fill = ServiceLine), size = 1.2) +
          geom_hline(yintercept = 100, color = "red", linetype = "dashed", size = 1.05) +
          geom_hline(yintercept = 90, color = "grey40", linetype = "dashed", size = 1.05) +
          scale_x_date(date_labels = "%b '%y", date_breaks = breaks) +
          theme_minimal() +
          labs(title = "% of Capacity",
               subtitle = "Dashed Lines are 100% and 90% Utilization Rates",
               x = input$aggregation_level, y = "Percentage of Capacity") +
          theme(
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
            axis.text.y = element_text(size = 12),
            plot.title = element_text(size = 18, face = "bold"),
            legend.position = "none"
          )
      })
      
      # Return the card UI
      column(
        width = 4,
        card(
          card_header(sl, class = "bg-primary"),
          card_body(
            plotOutput(xname, height = "150px"),
            plotOutput(mrname, height = "150px"),
            plotOutput(cyo_barname, height = "150px")
          )
        )
      )
    })
    
    do.call(fluidRow, Filter(Negate(is.null), cards))
  })
}

  #==== Examine Data Sets for Debugging ====#
  # 
  # # Preview: data
  # output$data_preview11 <- renderPrint({
  #   req(spc_data_cyo())
  #   head(spc_data_cyo(), 10)
  # })

#######################################################################################################################
#                                                       End                                                           #
#######################################################################################################################
