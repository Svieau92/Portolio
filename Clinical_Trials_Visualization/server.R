#########################################################################################################

########## Load Libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(forcats) # For reversing categories for plots
library(plotly) # For interactive plots
library(safetyGraphics) # Rho visualizations
library(shinyWidgets) # For cooler icons
library(DT) # For outputting data tables

#########################################################################################################

########## Load Data Sets ##########

# Load accrual data set
accrual <- read_csv(r"(C:\Users\sviea\Documents\Portfolio\Data_Visualization\Clinical_Trials\Data\dashboard-accrual.csv)")

# Load Subject Visits data set
sv <- read_csv(r"(C:\Users\sviea\Documents\Portfolio\Data_Visualization\Clinical_Trials\Data\sv.csv)")

# Load Demographics data set
dm <- read_csv(r"(C:\Users\sviea\Documents\Portfolio\Data_Visualization\Clinical_Trials\Data\dm.csv)")

# Load Adverse Events Data set
ae <- read_csv(r"(C:\Users\sviea\Documents\Portfolio\Data_Visualization\Clinical_Trials\Data\ae.csv)")

########## Manipulate Data sets ##########

# Filter accrual dataset to latest day per patient
accrual_summary <- accrual |>
  group_by(subjid) |> 
  filter(date == max(date)) |>
  ungroup()

# Duplicate for discontinuations plot
sv_sub <- sv |> 
  filter(SVSTATUS == "Terminated" | SVSTATUS == "Completed")

# Filter subject visits data set to not include unscheduled visits
# and remove terminated patients
sv <- sv |>
  filter(VISITNUM %in% c(0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0)) |>
  filter(SVSTATUS != "Terminated")

# Peform a merge so we can get the trial arm variable onto the SV domain
sv <- left_join(sv, select(dm, USUBJID, SITEID, ARM), by  = "USUBJID")

# Join SITE, SEX, and RACE, and REFENDY (subject days in trial) to AE domain
ae <- left_join(ae, select(dm, USUBJID, SITEID, SEX, RACE, RFENDY), by = "USUBJID")

# Clean up AETERM labels
table(ae$AEBODSYS)
ae <- ae |> 
  mutate(AEBODSYS = recode(AEBODSYS,
                           "Cardiac disorders" = "Cardiac",
                           "Ear and labyrinth disorders" = "Ear and Labryinth",
                           "Endocrine disorders" = "Endocrine",
                           "Eye disorders" = "Eye",
                           "Gastrointestinal disorders" = "Gastrointestinal",
                           "General disorders and administration sit conditions" = "General Disorders",
                           "General disorders and administration site conditions" = "General Disorders",
                           "Hepatobiliary disorders" = "Hepatobiliary",
                           "Immune system disorders" = "Immune System",
                           "Infections and infestations" = "Infections and Infestations",
                           "Injury, poisoning and procedura complications" = "Injury & Poisoning",
                           "Injury, poisoning and procedural complications" = "Injury & Poisoning",
                           "Metabolism and nutrition disorders" = "Metabolic",
                           "Musculoskeletal and connective tissu disorders" = "Musculoskeletal",
                           "Neoplasms benign, malignant an unspecified (incl cysts and polyps)" = "Neoplasms",
                           "Nervous system disorders" = "Nervous System",
                           "Pregnancy, puerperium and perinata conditions" = "Pregnancy Complications",
                           "Psychiatric disorders" = "Psychiatric",
                           "Renal and urinary disorders" = "Renal and Urinary",
                           "Reproductive system and breast disorders" = "Reproductive System",
                           "Respiratory, thoracic and mediastina disorders" = "Respiratory",
                           "Skin and subcutaneous tissue disorders" = "Skin & Tissue",
                           "Vascular disorders" = "Vascular",
                            "Blood and lymphatic system disorders" = "Blood and Lymphatic"))

#########################################################################################################

############ Define server logic

server <- function(input, output, session) {
  
  
  #########################################################################################################
  
  ########## DASHBOARD ##########

  ########### Accrual ##########

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
    p1a <- ggplot(accrual_summary_interactive(), aes(y = fct_rev(category_abbreviation),
                                                    fill = fct_rev(population),
                                                    text = paste0("Site: ", as.character(category_abbreviation),
                                                                  "<br>Population: ", population,
                                                                  "<br>Count: ", Population_Count,
                                                                  "<br>Percentage of Site Total: ", Population_Percentage, "%"),
                                                    customdata = paste(category_abbreviation, population, sep = "|")
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
    plotly_obj_1a <- ggplotly(p1a, tooltip = "text", source = "plot1a") |>
      layout(legend = list(
        title = list(),
        x = 0.5,
        xanchor = "center",
        y = -0.35
      )) |>
      config(displayModeBar = FALSE)  # Hide zoom/pan buttons

    # Manually update legend labels (to ensure proper naming)
    plotly_obj_1a$x$data <- lapply(plotly_obj_1a$x$data, function(trace) {
      if (!is.null(trace$name)) {
        if (trace$name == "Screened") {
          trace$name <- paste0("Screened: ", screened_total())
        } else if (trace$name == "Randomized") {
          trace$name <- paste0("Randomized: ", randomized_total(), " (", randomized_perc(), "%)")
        }
      }
      return(trace)
    })
    
    event_register(plotly_obj_1a, "plotly_click")  # Register click event
    plotly_obj_1a

  })
  
  
  
  
  
  
  
  ########## Accrual Over Time ##########
  
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
  
  # Plot hovered date object
  output$hovered_date <- renderText({
        hover_data <- event_data("plotly_hover", source = "plot1b")

        if (!is.null(hover_data) && "x" %in% names(hover_data)) {
          hover_date <- as.Date(hover_data$x, origin = "1970-01-01")  # Ensure proper conversion

          return(paste("Date:", unique(hover_date)))  # Filter unique values
        }
      })
  
  
  
  

  
  ########## Visit Completion ##########

  #### Create a reactive dataset based on user input
  sv_filtered <- reactive({
    
    # Reactivity based on site filter
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
    
    # Reactivity based on arm filter
    if (input$arm_filter1c != "All") {
      sv <- sv |> filter(ARM == input$arm_filter1c)
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
                                                     round(Percentage,2), "%"),
                                       customdata = paste(VISITNUM, SVSTATUS, sep  = "|"))) +
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
      sv_summary <- sv_filtered() |> 
        count(VISITNUM, SVSTATUS)
      
      # Now plot using the pre-summarized data
      p1c <- ggplot(sv_summary, aes(x = VISITNUM, y = n, fill = fct_rev(SVSTATUS),
                                    text = paste(SVSTATUS, ":", n),
                                    customdata = paste(VISITNUM, SVSTATUS, sep  = "|"))) +
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
    plotly_obj_1c <- ggplotly(p1c, tooltip = "text", source = "plot1c") |>
      layout(hovermode = "xxx",
        legend = list(
        title = list(),
        x = 0.5,
        xanchor = "center",
        y = -0.15,
        orientation = "h"
      )) |>
      config(displayModeBar = FALSE)  # Hide zoom/pan buttons
    
    event_register(plotly_obj_1c, "plotly_click")  # Register click event
    plotly_obj_1c
    
    
    
  })
  
  
  ########### Cross Connectivity Between Plots ##########
  
  observeEvent(event_data("plotly_click", source = "plot1a"), {
    click_info_1a <- event_data("plotly_click", source = "plot1a")
    clicked_vals_1a <- unlist(strsplit(click_info_1a$customdata, "\\|"))
    
    # Ensure click happened AND mode is "Filter by Site"
    if (!is.null(click_info_1a$customdata) && input$click_mode == "Filter by Site") {
      updateSelectInput(session, "site_filter1b", selected = clicked_vals_1a[1])
      updateSelectInput(session, "site_filter1c", selected = clicked_vals_1a[1])

      # Ensure arms of all plots match the arm filter of Plot 1A (accrual)      
      if (input$arm_filter1a == "Placebo") {
        updateSelectInput(session, "arm_filter1b", selected = "Placebo")
        updateSelectInput(session, "arm_filter1c", selected = "Placebo")
      }
      
      if (input$arm_filter1a == "Treatment A") {
        updateSelectInput(session, "arm_filter1b", selected = "Treatment A")
        updateSelectInput(session, "arm_filter1c", selected = "Treatment A")
      }
      
      if (input$arm_filter1a == "Treatment B") {
        updateSelectInput(session, "arm_filter1b", selected = "Treatment B")
        updateSelectInput(session, "arm_filter1c", selected = "Treatment B")
      }
      
      if (input$arm_filter1a == "Screen Failure") {
        updateSelectInput(session, "arm_filter1b", selected = "Screen Failure")
        updateSelectInput(session, "arm_filter1c", selected = "Screen Failure")
      }
      
      # Handle filtering when Drill Down button is selected and barplot is clicked
    } else if (input$click_mode == "Drill Down") {
      print("drill down")
      data_dashboard$data <- accrual_summary |> 
        filter(category_abbreviation == clicked_vals_1a[1]) |> 
        filter(population == clicked_vals_1a[2]) |> 
        select(-category_info) |> 
        group_by(subjid) |> 
        filter(date == max(date))
      
      # Handle matching of arm filters
      if (input$arm_filter1a == "Placebo") {
        data_dashboard$data <- data_dashboard$data |> 
          filter(`filter:Arm` == "Placebo")
      }
      
      if (input$arm_filter1a == "Treatment A") {
        data_dashboard$data <- data_dashboard$data |> 
          filter(`filter:Arm` == "Treatment A")
      }
      
      if (input$arm_filter1a == "Treatment B") {
        data_dashboard$data <- data_dashboard$data |> 
          filter(`filter:Arm` == "Treatment B")
      }
      
      if (input$arm_filter1a == "Screen Failure") {
        data_dashboard$data <- data_dashboard$data |> 
          filter(`filter:Arm` == "Screen Failure")
      }
    }
  })
  
  
  ########### Screened vs Target ##########
  
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
    p1d <- ggplot(total_screened, aes(x = value, 
                                      y = category)) +
      geom_col(aes(text = paste("Total Screened:", value)), 
               fill = "#A6CEE3") +
      labs(y = "Subjects Screened", x = NULL) + 
      theme_minimal() +
      theme(axis.text.y = element_blank(), 
            axis.ticks.y = element_blank(),
            axis.text.x = element_text(size = 14),
            axis.title.y = element_text(size = 14)) +
      geom_vline(xintercept = 150, linetype = "dotted", size = 1) +
      xlim(0,180) +
      geom_text(aes(x = 170, label = "Screened \nTarget"), hjust = 0, size = 3) +
      
      # Invisible point for tooltip
      geom_point(aes(x = 170, y = 1, text = "Target Goal: 150"), alpha = 0)
    
    # Convert to plotly object
     plotly_obj_1d <- ggplotly(p1d, tooltip = "text", source = "plot1d") |> 
      config(displayModeBar = FALSE)  # Hide zoom/pan buttons
       
    event_register(plotly_obj_1d, "plotly_click")  # Register click event
    plotly_obj_1d
  })
  

  
  
  
  
  
  ########## Randomized vs Target ##########
  
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
    xlim(0,130) +
    geom_text(aes(x = 120, label = "Randomized \nTarget"), hjust = 0, size = 3) +
    
    # Invisible point for tooltip
    geom_point(aes(x = 120, y = 1, text = "Target Goal: 100"), alpha = 0)
  
  # Convert to plotly object
  plotly_obj_1e <- ggplotly(p1e, tooltip = "text", source = "plot1e") |> 
    config(displayModeBar = FALSE)
  
  event_register(plotly_obj_1e, "plotly_click")  # Register click event
  plotly_obj_1e
  })
  
  
  
  
  
  
  
  ########## Discontinuations and Completions ##########
  
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
                                     text = ifelse(category == "Subjects Discontinued", paste("Total Discontinued:", value), paste("Total Completed:", value)),
                                     customdata = category)) +
    geom_col() +
    scale_fill_manual(values = c("Subjects Discontinued" = "darkgrey", "Subjects Completed" = "green4")) +
    theme_minimal() +
    labs(y = NULL, x = NULL, fill = NULL) +
    theme(axis.ticks.y = element_blank(),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          legend.position = "none")
  
  # Convert to plotly object
  plotly_obj_1f <- ggplotly(p1f, tooltip = "text", source = "plot1f") |> 
    config(displayModeBar = FALSE)
  
  
  event_register(plotly_obj_1f, "plotly_click")  # Register click event
  plotly_obj_1f
  
  })
  
  
  
  
  ########### Details-On-Demand Dataset - Dashboard ###########
  
  # Initialize Dashboard Data set
  data_dashboard <- reactiveValues(data = NULL)
  
  # Observe the Reset Button
  observeEvent(input$reset_dashboard, {
    # Reset all inputs to their default values
    updateSelectInput(session, "click_mode", selected = "Filter by Site")
    updateSelectInput(session, "arm_filter1a", selected = "All")
    updateSelectInput(session, "arm_filter1b", selected = "All")
    updateSelectInput(session, "site_filter1b", selected = "All")
    updateSelectInput(session, "arm_filter1c", selected = "All")
    updateSelectInput(session, "site_filter1c", selected = "All")
    updateSelectInput(session, "nperc_filter", selected = "N")
  
    # Reset data set
    data_dashboard$data <- NULL
  })
  
  # Observe click events
  
  observeEvent(event_data("plotly_click", source = "plot1c"), {
    
    click_info_1c <- event_data("plotly_click", source = "plot1c")
    clicked_vals_1c <- unlist(strsplit(click_info_1c$customdata, "\\|"))
    print(clicked_vals_1c[1])
    print(clicked_vals_1c[2])
    
    if (!is.null(click_info_1c)) {
      data_dashboard$data <- sv |> 
        filter(VISITNUM == clicked_vals_1c[1], SVSTATUS == clicked_vals_1c[2])
      
      # Apply ARM filter only if selection is not "All"
      if (input$arm_filter1c != "All") {
        data_dashboard$data <- data_dashboard$data |> filter(ARM == input$arm_filter1c)
      }
      
      # Apply SITE filter only if selection is not "All"
      if (input$site_filter1c != "All") {
        data_dashboard$data <- data_dashboard$data |> filter(SITEID == gsub("Site ", "", input$site_filter1c))
      }
      
      return(data_dashboard$data)
    }
  })
      

  
  # Click event for plot 1d
  observeEvent(event_data("plotly_click", source = "plot1d"), {
    click_info_1d <- event_data("plotly_click", source = "plot1d")
    if (!is.null(click_info_1d)) {
      data_dashboard$data <- accrual |> 
        filter(population == "Screened") |> 
        select(-category_info)
      
      
    }
  })
  
  # Click event for plot 1e
  observeEvent(event_data("plotly_click", source = "plot1e"), {
    click_info_1e <- event_data("plotly_click", source = "plot1e")
    if (!is.null(click_info_1e)) {
      data_dashboard$data <- accrual |> 
        filter(population != "Screened") |> 
        select(-category_info)
    }
  })

  # Click event for plot 1f
  observeEvent(event_data("plotly_click", source = "plot1f"), {
    click_info_1f <- event_data("plotly_click", source = "plot1f")
    
    if (!is.null(click_info_1f)) {
      customdata_value <- click_info_1f$customdata  # Access customdata
      
      if (customdata_value == "Subjects Discontinued") {
        data_dashboard$data <- sv_sub |> 
          arrange(USUBJID, SVDT) |> 
          group_by(USUBJID) |> 
          filter(SVSTATUS == "Terminated", lag(SVSTATUS) == "Completed")
      } else if (customdata_value == "Subjects Completed") {
        data_dashboard$data <- sv_sub |> 
          filter(SVSTATUS == "Completed", VISIT == "End of Study")  
      }
    }
  })

  # If reset is clicked, this ensures the datatable resets to show NULL
  output$drill_down_table_1 <- DT::renderDataTable({
    if (is.null(data_dashboard$data)) {
      return(data.frame(Message = "No data to display—click a bar to explore details"))
    }
    
    # Code generated by ChaptGPT to print all text in the data table in a single row
    DT::datatable(
      data_dashboard$data,
      extensions = "Buttons", 
      options = list(
        columnDefs = list(list(
          targets = "_all",
          render = JS(
            "function(data, type, row) {
            return '<div style=\"white-space: nowrap; overflow: hidden; text-overflow: ellipsis;\">' + data + '</div>';
          }"
          )
        )),
        scrollX = TRUE,
        dom = "frtipB",
        buttons = list(
          list(extend = "csv", text = "Download CSV")
        )
      ),
      class = "compact"
    )
  })
  

  
  
  
  
  
  
  
  
  #########################################################################################################
  
  ########## ADVERSE EVENTS ###########
  
  ########## Adverse Events Summary ###########
  
  # Create a reactive value for the details on demand dataset
  # Set it to default of null
  clicked_aebodysys_val <- reactiveVal(NULL)

  # Define the clicked_aebodsys reactive
  clicked_aebodysys <- reactive({
    clicked_aebodysys_val()
  })
  
  # Observe the Reset Button
  observeEvent(input$reset_ae, {
    # Reset all inputs to their default values
    updateSelectInput(session, "summarize_by", selected = "Events")
    updateSelectInput(session, "color_by", selected = "None")
    updateSelectInput(session, "severity", selected = "All")
    updateSelectInput(session, "outcome", selected = "All")
    updateSelectInput(session, "site_filter2", selected = "All")
    updateSelectInput(session, "serious", selected = "All")
    updateSelectInput(session, "related", selected = "All")
    updateSelectInput(session, "prevalence", selected = 0)
    
    # Reset clicked AEBODSYS
    clicked_aebodysys_val(NULL)
  })
  
  # Reactive for dynamic column selection
  color_by <- reactive({
    switch(input$color_by,
           "None" = NULL,
           "Severity" = "AESEV",
           "Outcome" = "AEOUT",
           "Sex" = "SEX",
           "Race" = "RACE",
           "Seriousness" = "AESER",
           "Relatedness" = "AEREL",
           NULL  # Default if input$color_by doesn't match any option
    )
  })
  
  # Filter and summarize dataset based on user input
  filtered_ae <- reactive({
    # Start with the original dataset
    data <- ae
    
    # Apply filters sequentially
    
    # Severity filter
    if (input$severity != "All") {
      data <- data |> filter(AESEV == toupper(input$severity))
    }
    
    # Outcome filter
    if (input$outcome != "All") {
      data <- data |> filter(AEOUT == toupper(input$outcome))
    }
    
    # Site filter
    if (input$site_filter2 != "All") {
      data <- data |> filter(SITEID == gsub("[^0-9]", "", input$site_filter2))
    }
    
    # Seriousness filter
    if (input$serious != "All") {
      data <- data |> filter(AESER == substring(input$serious, 1, 1))
    }
    
    # Relatedness filter
    if (input$related != "All") {
      data <- data |> filter(AEREL == toupper(input$related))
    }
    
    # Ensure column for color_by exists before grouping
    if (!is.null(color_by()) && !(color_by() %in% colnames(data))) {
      stop(paste("The column", color_by(), "does not exist in the dataset."))
    }
    
    # Calculate global total counts for AEBODSYS
    global_counts <- data |> 
      group_by(AEBODSYS) |> 
      summarise(
        total_count = if (input$summarize_by == "Patients") n_distinct(USUBJID) else n(),
        .groups = "drop"
      )
    
    # Apply Prevalence Filter to Overall Totals
    filtered_global <- global_counts |> 
      filter((total_count / sum(global_counts$total_count)) * 100 >= input$prevalence)
    
    # Keep only rows in `data` that match the filtered AEBODSYS values
    data <- data |> filter(AEBODSYS %in% filtered_global$AEBODSYS)
    
    # Group by color_by and AEBODSYS for finer counts
    grouped_data <- if (is.null(color_by())) {
      # No grouping by color_by column when "None" is selected
      data |> 
        group_by(AEBODSYS) |> 
        summarise(
          count = if (input$summarize_by == "Patients") n_distinct(USUBJID) else n(),
          .groups = "drop"
        )
    } else {
      # Include color_by column in grouping when selected
      data |> 
        group_by(AEBODSYS, .data[[color_by()]]) |> 
        summarise(
          count = if (input$summarize_by == "Patients") n_distinct(USUBJID) else n(),
          .groups = "drop"
        )
    }
    
    # Merge global ordering into grouped dataset
    grouped_data <- grouped_data |> 
      left_join(global_counts, by = "AEBODSYS") |>  # Add global total counts
      arrange(desc(total_count))  # Sort by global counts
    
    # Return grouped and ordered dataset for plotting
    return(grouped_data)
  })
  
  
  # Reactive for sorting and coloring barplot
  ae_sort <- reactive({
    # Prepare the dataset for plotting
    filtered_ae() |> 
      mutate(
        AEBODSYS = factor(AEBODSYS, levels = rev(unique(filtered_ae()$AEBODSYS))),  # Use the global sorted order
        fill_column = if (is.null(color_by())) "None" else .data[[color_by()]]  # Ensure consistent grouping
      )
  })
  
  
  # Render the plot
  output$plot2a <- renderPlotly({
    p2a <- ggplot(ae_sort(), aes(x = count, 
                                 y = AEBODSYS, 
                                 fill = fill_column,
                                 text = paste0(AEBODSYS, 
                                              "\nCount: ", count, " (", round((count/sum(ae_sort()$count))*100, 2), "%)",
                                              ifelse(fill_column == "None", "", paste("\n", fill_column))),
                                 customdata = paste(AEBODSYS, fill_column, sep = "|"))) +
      geom_bar(stat = "identity", position = "stack") +
      theme_minimal() +
      labs(
        title = paste0("Adverse Events Summary"),
        y = "Preferred Term",
        x = paste0(input$summarize_by, " Count", " (N = ", sum(ae_sort()$count), ")"),
        fill = input$color_by  # Dynamic legend label
      ) +
      theme(
        panel.grid = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        legend.spacing.x = unit(0.01, "cm"),     # Reduce spacing between items
        plot.title = element_text(size = 17),
        plot.title.position = "plot",
        fill.position = "bottom"
      ) +
      scale_fill_brewer(palette = "Pastel2")
    
    # Create interactive plot
    plotly_obj_2a <- ggplotly(p2a, tooltip = "text", source = "plot2a") |>  
      config(displayModeBar = FALSE) |>  
      layout(
        legend = list(
          orientation = "h",
          y = -0.2,
          x = 0.5,
          xanchor = "center",
          traceorder = "grouped",  # Group legend items by their categories
          valign = "middle"        # Align items neatly in rows
        )
      )
    
    # Register click event separately (after processing plotly_obj_2a)
    event_register(plotly_obj_2a, "plotly_click")  # Ensures event registration
    
    # Return the final object
    plotly_obj_2a
    
      })
  
  
  
  
  
  
  
  
  ########## Details-On-Demand Data Set  - Adverse Events ##########
  
  # Initialize
  clicked_filter_val <- reactiveVal(NULL)  # Initialize clicked filter reactive value
  
  # Observe event, update clicked_aebodysys_val to bar click
  observeEvent(event_data("plotly_click", source = "plot2a"), {
    click_info_2a <- event_data("plotly_click", source = "plot2a")
    if (!is.null(click_info_2a)) {
      clicked_vals <- unlist(strsplit(click_info_2a$customdata, "\\|")) # Split string into two values to separate the AEBODYSYS from the color filter
      
      # First value gets assigned to clicked_aebodysys_val
      clicked_aebodysys_val(clicked_vals[1])  
      
      # Only assign `clicked_filter_val` if color_by is NOT "None" or NULL
      if (!is.null(color_by()) && color_by() != "None") {
        clicked_filter_val(clicked_vals[2])  # Second value: Fill category (e.g., Severity)
      } else {
        clicked_filter_val(NULL)  # Reset if no color_by filter is applied
      }
      
      }
  })
  
  # Create filtered data set based on clicking of AE Summary bars
  drill_down_data_ae <- reactive({
    if (is.null(clicked_aebodysys_val())) {
      return(NULL)  # If nothing is clicked, return NULL
    }
    
    # Start filtering base dataset
    filtered_data_ae <- ae |> filter(AEBODSYS == clicked_aebodysys_val())
    
    # Apply color_by filter if selected
    if (!is.null(clicked_filter_val()) && color_by() != "None") {
      filtered_data_ae <- filtered_data_ae |> 
        filter(.data[[color_by()]] == clicked_filter_val())
    }
    
    # Apply ARM filter only if selection is not "All"
    if (input$severity != "All") {
      filtered_data_ae <- filtered_data_ae |> filter(AESEV == toupper(input$severity))
    }
    
    # Apply OUTCOME filter only if selection is not "All"
    if (input$outcome != "All") {
      filtered_data_ae <- filtered_data_ae |> filter(AEOUT == toupper(input$outcome))
    }
    
    # Apply SERIOUSNESS filter only if selection is not "All"
    if (input$serious != "All") {
      filtered_data_ae <- filtered_data_ae |> filter(AESER == substr(input$serious, 1, 1))
    }
    
    # Apply RELATEDNESS filter only if selection is not "All"
    if (input$related != "All") {
      filtered_data_ae <- filtered_data_ae |> filter(AEREL == toupper(input$related))
    }
    
    return(filtered_data_ae)  # Ensure filtered data is returned
  })
  
  
  # If reset is clicked, this ensures the datatable resets to show NULL
  output$drill_down_table_2 <- DT::renderDataTable({
    if (is.null(drill_down_data_ae())) {
      return(data.frame(Message = "No data to display—click a bar to explore details"))
    }

    # Code generated by ChaptGPT to print all text in the data table in a single row
    DT::datatable(
      drill_down_data_ae(),
      extensions = "Buttons", 
      options = list(
        columnDefs = list(list(
          targets = "_all",
          render = JS(
            "function(data, type, row) {
            return '<div style=\"white-space: nowrap; overflow: hidden; text-overflow: ellipsis;\">' + data + '</div>';
          }"
          )
        )),
        scrollX = TRUE,
        dom = "frtipB",
        buttons = list(
          list(extend = "csv", text = "Download CSV")
        )
      ),
      class = "compact"
    )
  })
  
  
  
  
  
  
  

  ########## Adverse Events by Site ###########

  # Create data set with values for total subject weeks and AE rate per site
  sites <- ae |> 
    group_by(SITEID) |> 
    summarize(total_subj_weeks = sum(RFENDY)/7,
              ae_total = n(),
              ae_rate = ae_total/total_subj_weeks) 
  
  # Calculate average AE rate across all sites
  avg_ae_rate <- mean(sites$ae_rate)
  
  # Reactive function for clicked site from AE by Site plot
  clicked_site <- reactive({
    click_info_2b <- event_data("plotly_click", source = "plot2b")  # Capture click event
    if (!is.null(click_info_2b)) {
      return(click_info_2b$customdata)  # Extract SITEID from customdata
    }
    return(NULL)  # Return NULL if no point is clicked
  })
  
  # Reactive function to determine the active site
  active_site <- reactive({
    site_from_dropdown <- input$site_filter2
    site_from_click <- clicked_site()
    
    # Priority: use the dropdown selection if not "All", otherwise use the click
    if (site_from_dropdown != "All") {
      return(gsub("Site ", "", site_from_dropdown))  # Extract SITEID from input
    } else if (!is.null(site_from_click)) {
      return(site_from_click)
    }
    
    return(NULL)  # No active site
  })
  
  # Observe filter-on-click for SITEID to dynamically update the dropdown filter
  # For AE By Site plot
  observeEvent(clicked_site(), {
    site <- clicked_site()  # Get the clicked site ID
    if (!is.null(site)) {
      updateSelectInput(session, inputId = "site_filter2", selected = paste("Site", site))  # Update dropdown with clicked site
    }
  })
  
  # Render AE by Site plot
  output$plot2b <- renderPlotly({
    # Calculate alpha values for each site
    sites$alpha <- if (input$site_filter2 == "All" || is.null(active_site())) {
      1  # Default: show all points fully opaque
    } else {
      ifelse(sites$SITEID == active_site(), 1, 0.3)  # Highlight active site
    }
    
    # Create the ggplot object
    p2b <- ggplot(sites, aes(x = total_subj_weeks, 
                             y = ae_rate, 
                             color = ae_rate,
                             text = paste0("Clinical Site: ", SITEID,
                                           "\nAE Rate: ", round(ae_rate, 3)),
                             customdata = SITEID,
                             alpha = alpha)) +
      geom_point(shape = 18, size = 4) +
      geom_hline(yintercept = avg_ae_rate, linetype = "dashed") +
      theme_minimal() +
      labs(title = "Adverse Events by Site",
           x = "Total Subject Weeks",
           y = "AE/Total Subject Weeks",
           color = "AE Rate") + 
      annotate("text", x = 3000, y = avg_ae_rate + 0.0002, label = paste("AVG AE Rate = ", round(avg_ae_rate, 3))) +
      xlim(1800, 3250) +
      scale_color_gradient2(low = "orange", mid = "grey", high = "orange", midpoint = avg_ae_rate) + 
      scale_alpha_identity() +  # Ensure ggplot respects alpha values
      theme(
        panel.grid = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        legend.key.size = unit(0.3, "cm"),
        plot.title = element_text(size = 17)
      )
    
    # Convert ggplot to Plotly object
    plotly_obj_2b <- ggplotly(p2b, tooltip = "text", source = "plot2b") |> 
      config(displayModeBar = FALSE)
    event_register(plotly_obj_2b, "plotly_click")  # Register click event
    plotly_obj_2b
  })
  
}







########################## TO DO
# Right now for my accrual bar, the tooltip is split between statuses. I want to make it a single tooltip.
#       Alternatively, change the number for count to be randomized + screened, so it shows the total).
# Also add drill down functionality to accrual plot 1a
# 
# Maybe make visit completion bar chart to be hovermode = "xxx" instead of "x"
#
# Here's what I think I want my interactivity for the dashboard to be:
#       Whenever you click a bar on the accrual chart
#       it sets accrual over time to whatever arm filter the accrual chart is on
#       and then sets the site for accrual over time to the site that got clicked
#       and changes the site in the visit progression plot to the site that got clicked.
#
# AE summary plot
#   Shift the filters to be in an order I prefer (Site at top or higher up)
#       A small multiples option for the sites would be cool. Easier to see them side by side
#       compared to having to click between them.
################################
