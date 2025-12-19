#######################################################################################################################
# App Name: Division of Hospital Medicine Patient Volume Visualizer                                                   #
# Author: Sean Vieau                                                                                                  #
# Date Created: 9/29/2025                                                                                             #
# Last Updated:                                                                                                       #
#                                                                                                                     #
# Info: The purpose of this app is to aid the CU DHM in visualizing patient volume and capacity.                      #
#       Users are able to see in real time patient volumes and capacity on the service line and team levels           #                                                                                  
#       And are provided with metrics and plots that flag groups that are at risk of being overburdened               #                                                                                                             
#                                                                                                                     #
#       This is the ui.R file, where we define what the app will actually look like                                   #
#######################################################################################################################

#######################################################################################################################
#                                                     Define UI                                                       #
#######################################################################################################################

# Define navigation bar
page_navbar(
  
  # Add the Title and CU Logo
  title = tags$div(
    style = "display: flex; align-items: center; height: 100%;",
    tags$img(src = "logo.png", style = "height: 40px; margin-right: 5px;"),
    tags$span("Hospital Volume Visualizer", style = "font-size: 1.25rem; line-height: 1; display: flex; align-items: center;")
  ),
  
  # Controls the colors of the panels. Sets selected panel text to be gold when selected
  header = tags$style(HTML("
    .nav-link.active {
      color: #CFB87C !important;
    }
    .navbar-brand {
      display: flex;
      align-items: center;
      height: 100%;
    }
  ")),
  
  #######################################################################################################################
  #                                                 Upload Data                                                         #
  #######################################################################################################################
  
  # Create panel
  nav_panel(
    title = "Upload Data",
    
    # Accept user selection for Census data set
    fileInput("file", "📂 Upload census workbook (.xlsx)"),
    
    # Accept user selection for Daily Capacities file
    fileInput("caps", "📁 Upload daily caps file (.xlsx)"),
    
    # Click here when ready to process data
    actionButton("process", "🚀 Process Data"),

    # Show message when datasets are finished processing
    withSpinner(textOutput("process_message"), type = 6, color = "#CFB87C"),
    
    # # Note: Below is code to show the header of every data set used in the App.
    # #       This is very helpful for debugging so you can actually see what each data set looks like
    # #       as you work with it.
    # # 
    # #       Therefore I am leaving this in the code to make future debugging easier!
    # tags$br(), 
    # tags$br(), 
    # "Daily Volumes Data:",
    # withSpinner(verbatimTextOutput("data_preview2"), type = 6, color = "#CFB87C"),
    # "Capacity Data:",
    # withSpinner(verbatimTextOutput("data_preview3"), type = 6, color = "#CFB87C"),
    # "Final Combined Data:",
    # withSpinner(DTOutput("data_preview"), type = 6, color = "#CFB87C"),
    # # "Data_Months",
    # withSpinner(DTOutput("data_preview4"), type = 6, color = "#CFB87C"),
    # "Data_serviceline",
    # withSpinner(DTOutput("data_preview5"), type = 6, color = "#CFB87C"),
    # "data_serviceline_month",
    # withSpinner(verbatimTextOutput("data_preview6"), type = 6, color = "#CFB87C"),
    # "data_team",
    # withSpinner(verbatimTextOutput("data_preview7"), type = 6, color = "#CFB87C"),
    # "data_team_month",
    # withSpinner(verbatimTextOutput("data_preview8"), type = 6, color = "#CFB87C"),
    # "cyop data",
    # withSpinner(DTOutput("data_preview9"), type = 6, color = "#CFB87C"),
    # "spc data",
    # withSpinner(verbatimTextOutput("data_preview10"), type = 6, color = "#CFB87C"),
    # "spc cyop data",
    # withSpinner(DTOutput("data_preview11"), type = 6, color = "#CFB87C"),
    # "Total_plot_data",
    # withSpinner(DTOutput("data_preview12"), type = 6, color = "#CFB87C")
  ),
  
#######################################################################################################################
#                                                     About                                                           #
#######################################################################################################################
  
  # Create About section panel
  nav_panel(
    title = "About",
    HTML("
      <h3 style='text-align: center;'>About This App</h3>
      <p>The Hospital Patient Volume Visualizer is an RShiny application designed to help users understand and monitor patient volumes across service lines and teams. Built on five years of daily census data, the tool supports proactive staffing decisions by tracking volumes, capacities, and utilization rates.</p>
      
      <p>This app was developed for my Master's Practicum project. All values have been randomized and noise has added to ensure privacy while preserving analytical utility. Likewise, team names have been made generic to the general hospital setting.</p>
      
      <p><strong>Purpose:</strong> The app enables the user to provide enhanced care to patients by offering a data visualization dashboard that highlights operational health and emerging risks.</p>
  
      <p><strong>Goals:</strong> By reviewing plots and metrics over user-selected time periods, leaders can:</p>
      <ul>
        <li>Identify short-term trends in patient volumes and spot shifts in demand</li>
        <li>Monitor current operational health, including volumes and capacities for each team</li>
        <li>Flag teams or service lines at risk of exceeding 90% or 100% capacity</li>
        <li>Anticipate upcoming surges in patient volume and adjust staffing before escalation</li>
      </ul>
  
      <h4>Visualizations</h4>
      <ul>
        <li><strong>Volume Line Plots:</strong> Show daily patient volumes with capacity overlays to highlight surges.</li>
        <li><strong>Capacity Bar Plots:</strong> Express volumes as % of capacity, with reference lines at 90% and 100%.</li>
        <li><strong>Change Plots:</strong> Display day-to-day increments or decrements in patient volume.</li>
        <li><strong>SPC X Chart:</strong> Statistical Process Control chart showing average volumes and control limits.</li>
        <li><strong>SPC Moving Range Chart:</strong> Highlights variability between successive observations.</li>
      </ul>
  
        <h5>Top Line Metrics</h5>
    <ul>
      <li><strong>Average % Capacity Used (vs. Previous Look Back Period):</strong> Shows change in average utilization compared to the prior period.</li>
      <li><strong>Number of Service Lines >90% Capacity:</strong> Counts service lines operating above 90% capacity on average.</li>
      <li><strong>Number of Teams >90% Capacity:</strong> Counts teams operating near or above critical thresholds.</li>
    </ul>

    <h5>Group Level Metrics</h5>
    <ul>
      <li><strong>Average Daily Volume:</strong> Mean number of patients seen per day across the look-back period.</li>
      <li><strong>Average Capacity:</strong> Mean available capacity over the same period.</li>
      <li><strong>Average % Capacity:</strong> (Average Daily Volume ÷ Average Capacity) × 100.</li>
      <li><strong>Days >90% Capacity:</strong> Number of days a group operated above 90% capacity.</li>
      <li><strong>Consecutive Days >90% Capacity:</strong> Longest streak of sustained high utilization days.</li>
      <li><strong>Days at 100% Capacity:</strong> Flags days when a group was fully saturated.</li>
    </ul>

  
      <h4>Design Philosophy</h4>
      <p>The app follows Munzner’s visualization principles: <em>overview first, zoom and filter, then details on demand</em>. Users move from high-level operational summaries down to granular team-level detail, with interactive cards and drill-down options for deeper exploration.</p>
    ")
  ),
  
#######################################################################################################################
#                                                   Dashboard                                                         #
#######################################################################################################################  

# Create Panel for Dashboard
nav_panel(
  title = "Dashboard",
  
  #==============================================================================================#
  #                                          Sidebar                                             #                    
  #==============================================================================================#

  # Create sidebar
  layout_sidebar(
    sidebar = sidebar(
      bg = "lightgrey",
      
      # Reset button
      actionBttn("reset_dash", "Reset", style = "unite", size = "md", color = "warning", icon = icon("undo")),
      
       # Look back period filter
      pickerInput(
        inputId = "look_back_period",
        label = "Look-Back Period:",
        choices = c("1 Week", "2 Weeks", "1 Month", "3 Months", "6 Months", "1 Year"),
        selected = "1 Month"
        )
    ),
    
    #==============================================================================================#
    #                                     Global Metrics                                           #                    
    #==============================================================================================#
    
    # % of Capacity Used compared to Previous Window
    fluidRow(
      column(4,
             div(
               style = "height: 150px; border: 1px solid var(--bs-primary); border-radius: 8px; text-align: center; padding-top: 30px;",
               tags$i(class = "fa-solid fa-bed-pulse fa-2x text-primary"),
               tags$br(),
               tags$strong("% Capacity Used (vs Previous Window)"),
               tags$br(),
               textOutput("avg_perc_cap_delta")
             )
    ),
    
    # Number of Service lines above 90% Average Capacity 
      column(4,
             div(
               style = "height: 150px; border: 1px solid var(--bs-primary); border-radius: 8px; text-align: center; padding-top: 30px;",
               tags$i(class = "fa-solid fa-hospital fa-2x text-primary"),
               tags$br(),
               tags$strong("Service Lines Above 90% Avg Capacity"),
               tags$br(),
               textOutput("above_90_count")
             )
      ),
    
    # Number of Teams above 90% Average Capacity 
      column(4,
             div(
               style = "height: 150px; border: 1px solid var(--bs-primary); border-radius: 8px; text-align: center; padding-top: 30px;",
               tags$i(class = "fa fa-house-medical fa-2x text-primary"),
               tags$br(),
               tags$strong("Teams Above 90% Avg Capacity"),
               tags$br(),
               textOutput("above_90_count_teams")
             )
      )
      ),
    
    #==============================================================================================#
    #                                  Inpatient Medicine Total                                    #                    
    #==============================================================================================#
    
    # Title of Section
    fluidRow(
      column(12,
             div("Overall Metrics",
                 style = "background-color: white; color: #CFB87C; padding: 10px 20px; font-weight: bold; font-size: 1.2rem; border-bottom: 1px solid #ddd;"
             )
      )
    ),
    
    # Create Inpatient Medicine Total Card
    fluidRow(
      column(
        width = 12,
        
        # Create title
        card(
          style = "height: 780px;",
          card_header("Inpatient Medicine Total", class = "bg-primary"),
          card_body(
            tagList(
              
              # Create top row: 6 metric tiles
              fluidRow(
                
                # Average Total Volume
                column(
                  width = 2,
                  div(
                    style = "background-color: #f8f9fa; border-radius: 6px; padding: 0.4em; height: 80px; display: flex; flex-direction: column; justify-content: center; align-items: center;",
                    tags$div("Avg Daily Volume", style = "font-size: 0.75em; color: #6c757d;"),
                    tags$div(textOutput("avg_vol_total"), style = "font-size: 1.1em; font-weight: bold;")
                  )
                ),
                
                # Average Total Available
                column(
                  width = 2,
                  div(
                    style = "background-color: #f8f9fa; border-radius: 6px; padding: 0.4em; height: 80px; display: flex; flex-direction: column; justify-content: center; align-items: center;",
                    tags$div("Avg Available", style = "font-size: 0.75em; color: #6c757d;"),
                    tags$div(textOutput("avg_avail_total"), style = "font-size: 1.0em; font-weight: bold;")
                  )
                ),
                
                # Average % of Capacity
                column(
                  width = 2,
                  div(
                    style = "background-color: #f8f9fa; border-radius: 6px; padding: 0.4em; height: 80px; display: flex; flex-direction: column; justify-content: center; align-items: center;",
                    tags$div("Avg % Capacity", style = "font-size: 0.75em; color: #6c757d;"),
                    tags$div(textOutput("avg_perc_cap_total"), style = "font-size: 1.1em; font-weight: bold;")
                  )
                ),
                
                # Number of Days > 90% Capacity
                column(
                  width = 2,
                  div(
                    style = "background-color: #f8f9fa; border-radius: 6px; padding: 0.4em; height: 80px; display: flex; flex-direction: column; justify-content: center; align-items: center;",
                    tags$div("Days > 90%", style = "font-size: 0.75em; color: #6c757d;"),
                    tags$div(textOutput("n_days_90_total"), style = "font-size: 1.1em; font-weight: bold;")
                  )
                ),
                
                # Number of Consecutive Days > 90% Capacity
                column(
                  width = 2,
                  div(
                    style = "background-color: #f8f9fa; border-radius: 6px; padding: 0.4em; height: 80px; display: flex; flex-direction: column; justify-content: center; align-items: center;",
                    tags$div("Consecutive Days > 90%", style = "font-size: 0.75em; color: #6c757d;"),
                    tags$div(textOutput("n_consec_90_total"), style = "font-size: 1.1em; font-weight: bold;")
                  )
                ),
                
                # Number of Days at 100% Capacity
                column(
                  width = 2,
                  div(
                    style = "background-color: #f8f9fa; border-radius: 6px; padding: 0.4em; height: 80px; display: flex; flex-direction: column; justify-content: center; align-items: center;",
                    tags$div("Days at 100%", style = "font-size: 0.75em; color: #6c757d;"),
                    tags$div(textOutput("n_days_100_total"), style = "font-size: 1.1em; font-weight: bold;")
                  )
                )
              ),
              
              # First row plots
              fluidRow(
              
                # Volume Line Plot
                column(width = 4, plotlyOutput("total_plot_vol", height = "280px")),
                
                # Capacity Bar Plot
                column(width = 4, plotlyOutput("total_plot_bar", height = "280px")),
                
                # Volume Change Plot
                column(width = 4, plotlyOutput("total_plot_change", height = "280px"))
              ),
              
              # Second row plots
              fluidRow(
                
                # SPC X Chart
                column(width = 6, plotlyOutput("x_chart_total", height = "260px")),
                
                # SPC Moving Range Chart
                column(width = 6, plotlyOutput("mr_chart_total", height = "260px"))
              )
            )
          )
        )
      )
    ),
      
    #==============================================================================================#
    #                                    Service Line Metrics                                      #                    
    #==============================================================================================#
    
    # Create Title for Section
    fluidRow(
      column(12,
             div("Service Line Metrics",
                 style = "background-color: white; color: #CFB87C; padding: 10px 20px; font-weight: bold; font-size: 1.2rem; border-bottom: 1px solid #ddd;"
             )
      )
    ),
    
    # Call logic to create cards for each Service Line 
    # (Note: This logic is defined in Server.R, search for "output$card_layout")
    uiOutput("card_layout"),
  ),
),


#######################################################################################################################
#                                                   Historical Trends                                                 #
####################################################################################################################### 
#  NOTE: The Historical Trends panel, where the user can look back volume and capacity trends for  previous years,
#         is a future section I would like to add. I will keep the skeleton of the code here for future iterations
#         So that we can more easily implement this panel in the future.
# 
# # Create Panel for Historical Trends
# nav_panel(
#   title = "Historical Trends",
#   
#   #==============================================================================================#
#   #                                          Sidebar                                             #                    
#   #==============================================================================================#
#   
#   # Create sidebar
#   layout_sidebar(
#     sidebar = sidebar(
#       bg = "lightgrey",
#       
#       # Reset button
#       actionBttn("reset", "Reset", style = "unite", size = "md", color = "warning", icon = icon("undo")),
#       
#       # Date Range Filter
#       dateRangeInput("date_range_selector", label = "Select Date Range:", 
#                      start = as.Date(paste0(as.numeric(format(Sys.Date(), "%Y")) - 1, "-07-01")),
#                      end   = as.Date(paste0(format(Sys.Date(), "%Y"), "-06-30")),
#                      format = "yyyy-mm-dd"),
#       
#       # Academic Year filter
#       checkboxGroupInput(
#         inputId = "AY_selector",
#         label = "Select Academic Year:",
#         choices = sheets,
#         selected = tail(sheets, 1)
#       ),
#       
#       # Service line filter
#       uiOutput("service_line_selector_ui"),
#       
#       # Team filter
#       uiOutput("team_selector_ui"),
#       
#     ),
#     
#     #==============================================================================================#
#     #                                     Top Line Metrics                                         #                    
#     #==============================================================================================#
#     
#     # Create card for top line metrics
#     # (Note: These are just hard coded placeholders for now. Can change or delete as needed)
#     fluidRow(
#       
#       # Service Lines above x Capacity
#       column(4,
#              div(
#                style = "height: 150px; border: 1px solid var(--bs-primary); border-radius: 8px; text-align: center; padding-top: 30px;",
#                tags$i(class = "fa fa-hospital fa-2x text-primary"),
#                tags$br(),
#                tags$strong("Service Lines Above 80% Capacity"),
#                tags$br(),
#                "4"
#              )
#       ),
#       
#       # Teams above x Capacity
#       column(4,
#              div(
#                style = "height: 150px; border: 1px solid var(--bs-primary); border-radius: 8px; text-align: center; padding-top: 30px;",
#                tags$i(class = "fa fa-chart-line fa-2x text-primary"),
#                tags$br(),
#                tags$strong("Teams Above 80% Capacity"),
#                tags$br(),
#                "14"
#              )
#       ),
#       
#       # Room for other useful metrics
#       column(4,
#              div(
#                style = "height: 150px; border: 1px solid var(--bs-primary); border-radius: 8px; text-align: center; padding-top: 30px;",
#                tags$i(class = "fa fa-user-md fa-2x text-primary"),
#                tags$br(),
#                tags$strong("Other Cool Metric"),
#                tags$br(),
#                "1.618"
#              )
#       )
#     ),
#     
#     # Fill Academic Year Plots
#     div(
#       style = "display: flex; flex-direction: column; height: 100%;",
#       card(
#         card_header("Academic Year Plots", class = "bg-primary"),
#         card_body(
#           fluidRow(
#             column(12, plotOutput("AY_vol", height = "500px")),
#             column(12, plotOutput("AY_perc", height = "500px"))
#           )
#         )
#       )
#     ),
#     
#     #==============================================================================================#
#     #                                       Create Plots                                           #                    
#     #==============================================================================================#
#     
#     # Create Service Line Plots
#     div(
#       style = "display: flex; flex-direction: column; height: 100%;",
#       card(
#         card_header("Service-Level Plots", class = "bg-primary"),
#         card_body(
#           fluidRow(
#             
#             # Volume Line Plots
#             column(12, plotOutput("service_vol", height = "600px")),
#             
#             # Capacity Bar Plots
#             column(12, plotOutput("service_perc", height = "600px"))
#           )
#         )
#       )
#     ),
#     
#     # Create Team Level Plots
#     div(
#       style = "display: flex; flex-direction: column; height: 100%;",
#       card(
#         card_header("Team-Level Plots", class = "bg-primary"),
#         card_body(
#           fluidRow(
#             
#             # Volume Line Plots
#             column(12, plotOutput("team_vol", height = "1500px")),
#             
#             # Capacity Bar Plots
#             column(12, plotOutput("team_perc", height = "1500px"))
#           )
#         )
#       )
#     )
#   )
# ),

#######################################################################################################################
#                                                Create Your Own Plot                                                 #
#######################################################################################################################
  
  # Create panel
  nav_panel(
    title = "Create Your Own",
    
    #==============================================================================================#
    #                                          Sidebar                                             #                    
    #==============================================================================================#
    
    # Create sidebar
    layout_sidebar(
      sidebar = sidebar(
        bg = "lightgrey",
        
        # Reset button
        actionBttn("reset_cyo", "Reset", style = "unite", size = "md", color = "default", icon = icon("undo")),
        
        # Select date range
        dateRangeInput("date_range_selector_cyo", label = "Select Date Range:", 
                       start = "2021-01-01",
                       end   = Sys.Date(),
                       format = "yyyy-mm-dd"),
        
        # Aggregate by day, week, or month
        radioGroupButtons(
          inputId = "aggregation_level",
          label = "Aggregate By:",
          choices = c("Days", "Weeks", "Months"),
          selected = "Months",
          status = "primary",
          justified = TRUE,
          size = "sm",
          checkIcon = list(
            yes = icon("check"),
            no = icon("")
          )
        ),
        
        # Service line filter
        uiOutput("service_line_selector_cyo_ui"),
        
        # Toggle switch for turning capacity lines on/off
        materialSwitch(
          inputId = "capacity_toggle",
          label = "Show Capacities:",
          status = "primary",
          right = FALSE,
          value = TRUE
        ),
        
        # Create lil box for visual separation
        tags$fieldset(
          style = "border: 2px solid #ccc; padding: 10px; margin-bottom: 15px;",
          tags$legend("Custom Group Builder", style = "font-weight: bold; font-size: 16px; color: #000000;"),
          
          # Select Number of Custom Groups
          sliderInput("num_custom_groups", "How Many Custom Groups Do You Want?", min = 0, max = 5, value = 0),
          
          # Dynamic UI for group inputs. Create a new box for every Number of Custom Groups selected above
          # (Note: This logic is defined in Server.R, search for "output$custom_group_inputs")
          uiOutput("custom_group_inputs")
        )
       ),
      
      #==============================================================================================#
      #                                       Create Plots                                           #                    
      #==============================================================================================#

      # Create Card for Create-Your-Own-Plot section
      div(
        style = "display: flex; flex-direction: column;",
        card(
          card_header("Create Your Own Plot", class = "bg-primary"),
          card_body(
            fluidRow(
              
              # Create Main Plot
              column(12, plotlyOutput("plot_cyo", height = "500px"))
            )
          )
        )
      ),
      
      # Create Cards and fill plots dynamically for SPC and barplots based on selected groups
      # (Note: This logic is defined in Server.R, search for "output$spc_service_cards")
      div(
        style = "display: flex; flex-direction: column;",
        uiOutput("spc_service_cards")
      )
    )
  )
)

#######################################################################################################################
#                                                       End                                                           #
#######################################################################################################################