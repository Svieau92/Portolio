#########################################################################################################

########## Load Libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(forcats) # For reversing categories for plots
library(plotly)
library(safetyGraphics) # Rho visualizations

#########################################################################################################

########## Load Data Sets

# Load accrual data set
accrual <- read_csv(r"(C:\Users\sviea\Documents\Portfolio\Data_Visualization\Clinical_Trials\Data\dashboard-accrual.csv)")

# Load Subject Visits data set
sv <- read_csv(r"(C:\Users\sviea\Documents\Portfolio\Data_Visualization\Clinical_Trials\Data\sv.csv)")

# Load Demographics data set
dm <- read_csv(r"(C:\Users\sviea\Documents\Portfolio\Data_Visualization\Clinical_Trials\Data\dm.csv)")

# Load Adverse Events Data set
ae <- read_csv(r"(C:\Users\sviea\Documents\Portfolio\Data_Visualization\Clinical_Trials\Data\ae.csv)")

########## Manipulate Data sets

# Filter accrual dataset to latest day per patient
accrual_summary <- accrual |>
  group_by(subjid) |> 
  filter(date == max(date)) |>
  ungroup()

# Merge demographics with subject visits to get the SITEID variable
sv <- left_join(sv, select(dm, "USUBJID", "SITEID"), by = "USUBJID")

# Duplicate for discontinuations plot
sv_sub <- sv |> 
  filter(SVSTATUS == "Terminated" | SVSTATUS == "Completed")

# Filter subject visits data set to not include unscheduled visits
# and remove terminated patients
sv <- sv |>
  filter(VISITNUM %in% c(0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0)) |>
  filter(SVSTATUS != "Terminated")

#########################################################################################################

############ Define server logic

server <- function(input, output, session) {

  ########### Accrual

  # Create a reactive dataset based on user input
  accrual_filtered <- reactive({
    if (input$arm_filter1a != "All") {
      accrual_summary |> filter(`filter:Arm` == input$arm_filter1a)
    } else {
      accrual_summary
    }
  })

  # Compute population counts per site
  accrual_summary_interactive <- reactive({
    accrual_filtered() |>
      group_by(category_abbreviation) |>
      mutate(Total = n(),
             Population_Count = ave(rep(1, n()), population, FUN = sum),
             Population_Percentage = round((Population_Count / Total) * 100, 1)) |>
      ungroup()
  })

  # Compute totals for legend
  screened_total <- reactive({ nrow(accrual_summary_interactive()) })
  randomized_total <- reactive({ sum(accrual_summary_interactive()$population == "Randomized") })
  randomized_perc <- reactive({ round((randomized_total() / screened_total()) * 100, 1) })

  output$plot1a <- renderPlotly({

    # Create ggplot
    p1a <- ggplot(accrual_summary_interactive(), aes(
      y = fct_rev(category_abbreviation),
      fill = fct_rev(population),
      text = paste0("Site: ", as.character(category_abbreviation),
                    "<br>Population: ", population,
                    "<br>Count: ", Population_Count,
                    "<br>Percentage of Site Total: ", Population_Percentage, "%")
    )) +
      geom_bar() +
      labs(x = NULL, y = NULL, fill = NULL) +
      theme_minimal() +
      theme(legend.position = "bottom",
            axis.text.y = element_text(size = 14),
            axis.text.x = element_text(size = 14),
            legend.text = element_text(size = 12)) +
      scale_fill_brewer(palette = "Paired")

    # Convert to plotly and adjust legend positioning
    plotly_obj1a <- ggplotly(p1a, tooltip = "text") |>
      layout(legend = list(
        title = list(),
        x = 0.5,
        xanchor = "center",
        y = -0.35
      )) |>
      config(displayModeBar = FALSE)  # Hide zoom/pan buttons

    # Manually update legend labels (to ensure proper naming)
    plotly_obj1a$x$data <- lapply(plotly_obj1a$x$data, function(trace) {
      if (!is.null(trace$name)) {
        if (trace$name == "Screened") {
          trace$name <- paste0("Screened: ", screened_total())
        } else if (trace$name == "Randomized") {
          trace$name <- paste0("Randomized: ", randomized_total(), " (", randomized_perc(), "%)")
        }
      }
      return(trace)
    })

    plotly_obj1a

  })
  
  ########## Accrual Over Time
  
  output$plot1b <- renderPlotly({
    
    # Create a reactive dataset based on user input
    accrual_filtered1b <- reactive({
      
      if (input$arm_filter1b != "All") {
        accrual <- accrual |> filter(`filter:Arm` == input$arm_filter1b)
      }
      if (input$site_filter1b != "All") {
        accrual <- accrual |> filter(category_abbreviation == input$site_filter1b)
      }
      
      return(accrual)
    })
  
  # Aggregate and compute cumulative sum within each population
  accrual_over_time <- accrual_filtered1b() |> 
    arrange(date) |>  # Ensure chronological order
    group_by(population, date) |> 
    summarise(total_patients = n(), .groups = "drop") |>  # Count patients per date
    group_by(population) |> 
    mutate(cumulative_total = cumsum(total_patients)) |>  # Compute rolling sum
    ungroup() |> 
    complete(date, population, fill = list(total_patients = 0)) |>  # Ensure all dates exist for every population
    group_by(population) |> 
    fill(cumulative_total, .direction = "down")  # Carry forward the last known value

  
  # Create plot
  p1b <- ggplot(accrual_over_time, aes(x = date, 
                                       y = cumulative_total, 
                                       color = fct_rev(population),
                                       group = population,
                                       text = ifelse(population == "Screened", 
                                                     paste("Total Screened:", cumulative_total), 
                                                     paste("Total Randomized:", cumulative_total)))) +
    geom_line(size = 1) +  
    theme_minimal() +
    scale_color_brewer(palette = "Paired") +
    labs(y = NULL, x = NULL, color = NULL) + 
    theme(axis.ticks.y = element_blank(),
          axis.text.x = element_text(size = 9),
          axis.text.y = element_text(size = 14),
          legend.position = "bottom",
          legend.text = element_text(size = 14))
  
  
  # Convert ggplot to plotly object
  ggplotly(p1b, tooltip = "text", source = "plot1b") |> 
    layout(
      hovermode = "x unified",
      legend = list(
        title = list(),
        x = 0.5,
        xanchor = "center",
        y = -0.05,
        orientation = "h")
    ) |> 
    config(displayModeBar = FALSE)
  })
  
  output$hovered_date <- renderText({
        hover_data <- event_data("plotly_hover", source = "plot1b")

        if (!is.null(hover_data) && "x" %in% names(hover_data)) {
          hover_date <- as.Date(hover_data$x, origin = "1970-01-01")  # Ensure proper conversion

          return(paste("Date:", unique(hover_date)))  # Filter unique values
        }
      })
  
  ########## Visit Completion

  #### Create a reactive dataset based on user input
  sv_filtered <- reactive({
    
    # Reactivity base on site filter
    if (input$site_filter1c != "All") {
      if (input$site_filter1c == "Site 01") {
        sv <- sv |> filter(SITEID == "01")
      } else if (input$site_filter1c == "Site 02") {
        sv <- sv |> filter(SITEID == "02")
      } else if (input$site_filter1c == "Site 03") {
        sv <- sv |> filter(SITEID == "03")
      } else if (input$site_filter1c == "Site 04") {
        sv <- sv |> filter(SITEID == "04")
      } else if (input$site_filter1c == "Site 05") {
        sv <- sv |> filter(SITEID == "05")
      }
    }
    
    # Reactivity base on %/N filter
    if (input$nperc_filter == "%") {
      sv_summary <- sv |>
        group_by(VISITNUM, SVSTATUS) |>
        summarise(Count = n(), .groups = "drop") |>
        group_by(VISITNUM) |>
        mutate(Percentage = Count / sum(Count) * 100)
      return(sv_summary)
    } else {
      return(sv)  
    }
  })

  # Create Plot
  output$plot1c <- renderPlotly({
    
    # Create logic to create plot with % on y axis
    if (input$nperc_filter == "%") {
      p1c <- ggplot(sv_filtered(), aes(x = VISITNUM, 
                                       y = Percentage, 
                                       fill = fct_rev(SVSTATUS),
                                       text = paste0(as.character(SVSTATUS), ": ",
                                                     round(Percentage,2), "%"))) +
        geom_bar(stat = "Identity") +
        theme_minimal() +
        labs(x = NULL, y = NULL, fill = NULL) +
        scale_fill_manual(values = c("Completed" = "green4",
                                     "Expected" = "skyblue2",
                                     "Overdue" = "orange",
                                     "Missed" = "red",
                                     "Failed" = "black")) +
        scale_x_continuous(breaks = seq(floor(min(sv_filtered()$VISITNUM, na.rm = TRUE)),
                                        ceiling(max(sv_filtered()$VISITNUM, na.rm = TRUE)),
                                        by = 1),
                           labels = function(x) ifelse(x == 0, "Screen", as.character(x))) +
        theme(legend.position = "bottom",
              axis.text.y = element_text(size = 14),
              axis.text.x = element_text(size = 14),
              legend.text = element_text(size = 12))
    } else {
      
      # Compute counts manually for hover tooltip
      sv_summary <- sv_filtered() %>%
        count(VISITNUM, SVSTATUS)
      
      # Now plot using the pre-summarized data
      p1c <- ggplot(sv_summary, aes(x = VISITNUM, y = n, fill = fct_rev(SVSTATUS),
                                    text = paste(SVSTATUS, ":", n))) +
        geom_bar(stat = "identity") +
        theme_minimal() +
        labs(x = NULL, y = NULL, fill = NULL) +
        scale_fill_manual(values = c("Completed" = "green4",
                                     "Expected" = "skyblue2",
                                     "Overdue" = "orange",
                                     "Missed" = "red",
                                     "Failed" = "black")) +
        scale_x_continuous(breaks = seq(floor(min(sv_summary$VISITNUM, na.rm = TRUE)),
                                        ceiling(max(sv_summary$VISITNUM, na.rm = TRUE)), by = 1),
                           labels = function(x) ifelse(x == 0, "Screen", as.character(x))) +
        theme(legend.position = "bottom",
              axis.text.y = element_text(size = 14),
              axis.text.x = element_text(size = 14),
              legend.text = element_text(size = 12))
    }
    
    # Convert to plotly and adjust legend positioning
    plotly_obj1c <- ggplotly(p1c, tooltip = "text") |>
      layout(hovermode = "x",
        legend = list(
        title = list(),
        x = 0.5,
        xanchor = "center",
        y = -0.15,
        orientation = "h"
      )) |>
      config(displayModeBar = FALSE)  # Hide zoom/pan buttons
  })
  
  ########### Screened vs Target
  
  # Get current total number of subjects screened
  screened <- nrow(accrual_summary)
  
  # Make it a dataframe for plotting
  total_screened <- data.frame(
    category = "Screened Subjects",
    value = screened
  )
  
  # Create plot
  output$plot1d <- renderPlotly({
    
    # Create Plot
    p1d <- ggplot(total_screened, aes(x = value, y = category)) +
      geom_col(aes(text = paste("Total Screened:", value)), fill = "#A6CEE3") +
      labs(y = "Subjects Screened", x = NULL) + 
      theme_minimal() +
      theme(axis.text.y = element_blank(), 
            axis.ticks.y = element_blank(),
            axis.text.x = element_text(size = 14),
            axis.title.y = element_text(size = 14)) +
      geom_vline(xintercept = 150, linetype = "dotted", size = 1) +
      xlim(0,160) +
      geom_text(aes(x = 160, label = "Target"), hjust = 0, size = 3) +
      
      # Invisible point for tooltip
      geom_point(aes(x = 160, y = 1, text = "Target Goal: 150"), alpha = 0)
    
    # Convert to plotly object
    ggplotly(p1d, tooltip = "text") |> 
      config(displayModeBar = FALSE)  # Hide zoom/pan buttons
  })
  
  ########## Randomized vs Target
  
  output$plot1e <- renderPlotly({
  
  # Make it a dataframe for plotting
  total_randomized <- accrual_summary |> 
    group_by(population) |> 
    count() |> 
    filter(population == "Randomized")
  
  # Create Plot
  p1e <- ggplot(total_randomized, aes(x = n, y = population)) +
    geom_col(aes(text = paste("Total Randomized:", n)), fill = "#1F78B4") +
    labs(y = "Subjects Randomized", x = NULL) + 
    theme_minimal() +
    theme(axis.text.y = element_blank(), 
          axis.ticks.y = element_blank(),
          axis.text.x = element_text(size = 14),
          axis.title.y = element_text(size = 14)) +
    geom_vline(xintercept = 100, linetype = "dotted", size = 1) + 
    xlim(0,110) +
    geom_text(aes(x = 107, label = "Target"), hjust = 0, size = 3) +
    
    # Invisible point for tooltip
    geom_point(aes(x = 107, y = 1, text = "Target Goal: 100"), alpha = 0)
  
  # Convert to plotly object
  ggplotly(p1e, tooltip = "text") |> 
    config(displayModeBar = FALSE)
  })
  
  ########## Discontinuations and Completions
  
  output$plot1f <- renderPlotly({
    
  #  Get the total number of patients terminated/discontinued
  discontinued <- sv_sub |>
    filter(SVSTATUS == "Terminated") |> 
    distinct(USUBJID) |> 
    nrow()
  
  #  Get the total number of patients Completed all 7 visits
  completed <- sv_sub |> 
    filter(VISITNUM == 7.0) |> 
    filter(SVSTATUS == "Completed") |> 
    distinct(USUBJID) |> 
    nrow()
  
  # Make it a dataframe for plotting
  total_disc_comp <- data.frame(
    category = c("Subjects Discontinued", "Subjects Completed"),
    value = c(discontinued, completed)
  )
  
  # Create Plot
  p1f <- ggplot(total_disc_comp, aes(x = value, 
                                     y = category,
                                     fill = category,
                                     text = ifelse(category == "Subjects Discontinued", paste("Total Discontinued:", value), paste("Total Completed:", value)))) +
    geom_col() +
    scale_fill_manual(values = c("Subjects Discontinued" = "darkgrey", "Subjects Completed" = "green4")) +
    theme_minimal() +
    labs(y = NULL, x = NULL, fill = NULL) +
    theme(axis.ticks.y = element_blank(),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          legend.position = "none")
  
  # Convert to plotly object
  ggplotly(p1f, tooltip = "text") |> 
    config(displayModeBar = FALSE)
  })
}

########################## TO DO
# Right now for my accrual bar, the tooltip is split between statuses. I want to make it a single tooltip.
#       Alternatively, change the number for count to be randomized + screened, so it shows the total).
# Also add drill down functionality to accrual plot 1a
# 
# Maybe make visit completion bar chart to be hovermode = "xxx" instead of "x"
################################

# 
# 
# # 
# 
# library(shiny)
# library(plotly)
# library(ggplot2)
# 
# # Define UI
# ui <- fluidPage(
#   plotlyOutput("plot"),  # Main plot
#   div(style = "position:absolute; bottom:10px; right:10px; padding:5px; font-size:16px; background:#f8f8f8; border:1px solid #ccc;",
#       textOutput("hovered_date"))  # Separate hover box
# )
# 
# # Define Server Logic
# server <- function(input, output) {
#   # Create ggplotly object
#   output$plot1b <- renderPlotly({
#     ggplotly(p1b, tooltip = "text") |>
#       layout(
#         hovermode = "x unified",
#         legend = list(
#           title = list(),
#           x = 0.5,
#           xanchor = "center",
#           y = -0.05,
#           orientation = "h")
#       ) |>
#       config(displayModeBar = FALSE)
#   })
# 
#   output$hovered_date <- renderText({
#     hover_data <- event_data("plotly_hover")
# 
#     if (!is.null(hover_data) && "x" %in% names(hover_data)) {
#       hover_date <- as.Date(hover_data$x, origin = "1970-01-01")  # Ensure proper conversion
# 
#       return(paste("Date:", unique(hover_date)))  # Filter unique values
#     }
#   })
# }
# 
# # Run App
# shinyApp(ui, server)


