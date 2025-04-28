library(shiny)
library(bslib)
library(ggplot2)
library(tidyverse)
library(readxl)
library(shinyWidgets)
library(plotly)

# Simple function to capitlize first letter of a word, for the user options
capitalize_first <- function(x) {
  paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
}

page_navbar(
  title = "Clinical Trials Visualization App",
  theme = bs_theme(
    bootswatch = "shiny",
    primary = "orange"
  ),
  inverse = FALSE,
  
  nav_panel(
    title = "About",
    div(style = "text-align: center;", h1("About")),
    h3("Data"),
    HTML("<p>The data set used in this software is an anonymized version of the Phase II study to evaluate the <u>Safety and Efficacy of the Xanomeline 
          Transdermal Therapeutic System (TTS) in Patients with Mild to Moderate Alzheimer’s Disease</u>. It is regularly used in Clinical Research data modelling exercises as an authentic, real-world clinical trials data set.</p>"),
    HTML('<p>Data is available in both ADaM and SDTM CDISC formats and can be found at: <a href="https://github.com/RhoInc/data-library/tree/master/data/clinical-trials" target="_blank">Clinical Trials Data</a></p>'),
    h3("Visualizations"),
    HTML("<p>Selected visualizations were based on examples provided by <a href=https://rhoinc.github.io/viz-library/>Rho</a> and <a href=https://www.youtube.com/watch?v=uBRLhdDeyLw>Genentech</a></p>")
  ),
  
  nav_panel(
    title = "Monitor Study Progress",
    tabsetPanel(
      layout_columns(
        
        ########## Actual Vs Target Screened ##########
        card(card_header(h5("Actual vs. Target Subjects Screened")), plotlyOutput("plot1d")),
        
        ########## Actual Vs Target Randomized ##########
        card(card_header(h5("Actual vs. Target Subjects Randomized")), plotlyOutput("plot1e")),
        
        ########## Completions and Discontinuations ##########
        card(card_header(h5("Completions and Discontinuations")), plotlyOutput("plot1f"))
      )
      ,
      layout_columns(
        
        ########## Accrual ##########
        card(
          card_header(
            div(style = "display: flex; justify-content: space-between; align-items: center;",
                h5("Accrual"),
                
                 # Restrict radio button width to prevent excessive space
                 div(style = "margin-left: 25px; display: flex; align-items: center; width: 150px;",
                     radioButtons("click_mode", "Click Enables:", choices = c("Filter by Site", "Drill Down"),
                                  inline = TRUE, width = "150px")  # Set explicit width
                 ),
                
                # Outer wrapper ensures everything is aligned correctly
                div(style = "margin-left: 0px; display: flex; justify-content: flex-end; align-items: center; width: 100%; gap: 20px;",  
                    selectInput("arm_filter1a", "Arm:",  
                                choices = c("All", unique(accrual$`filter:Arm`)),  
                                selected = "All",  
                                width = "150px")
                )
            )
          ),
          
          card_body(
            div(style = "overflow-x: hidden; width: 100%;",  # Ensure no horizontal scrolling
                plotlyOutput("plot1a")
            )
          )
        ),
        ########## Accrual Over Time ##########
        card(
          card_header(
            div(style = "display: flex; justify-content: space-between; align-items: center;",
                h5("Accrual Over Time"),
                selectInput("arm_filter1b", "Arm:", 
                            choices = c("All", unique(accrual$`filter:Arm`)), 
                            selected = "All",
                            width = "220px"),
                selectInput("site_filter1b", "Site:",  
                            choices = c("All", "Site 01", "Site 02", "Site 03", "Site 04", "Site 05"), 
                            selected = "All",
                            width = "220px"))
          ),
          card_body(
            
            plotlyOutput("plot1b"),
            
            # Separate hover date box in the bottom-right corner
            div(style = "position:absolute; bottom:85px; right:10px; padding:10px; font-size:16px; 
              background:transparent; border:none;", 
                textOutput("hovered_date")),
          )
        ),
        
        ########## Visit Completion ##########
        card(
          card_header(
            div(style = "display: flex; justify-content: space-between; align-items: center;",
                h5("Visit Completion"),
                div(style = "margin-left: auto",
                selectInput("arm_filter1c", "Arm",
                            choices = c("All", unique(sv$ARM)), 
                            selected = "All",
                            width = "140px")),
                selectInput("site_filter1c", "Site:",  
                            choices = c("All", "Site 01", "Site 02", "Site 03", "Site 04", "Site 05"), 
                            selected = "All",
                            width = "150px"),
            div(style = "display: flex; justify-content: space-between; align-items: center;",
                radioButtons("nperc_filter", label = NULL,
                             choices = c("%", "N"),
                             select = "N",
                             inline = TRUE)
            ))
          ),
          card_body(
            plotlyOutput("plot1c")
          )
        )
      ),
      card_body(
        actionBttn("reset_dashboard", "Reset", style = "stretch", size = "m", icon = icon("undo"), color = "primary"),
      ),
      card_body(
        h4("Details-On-Demand", align = "center"),
        DT:::dataTableOutput("drill_down_table_1")
      )
      
    )
  ),
  
  
 
  
  
  
  
  
  ##############################################################################################
  
  ########### ADVERSE EVENTS ############
  
  ########## Adverse Events Summary ##########
  nav_panel(
    title = "Adverse Events",
    tabsetPanel(
      tabPanel("Adverse Events Explorer",
               layout_sidebar(
                 sidebar = sidebar(
                   bg = "lightgrey",
                   actionBttn("reset_ae", "Reset", style = "stretch", size = "sm", icon = icon("undo")),
                   HTML("<u>Choose Visual Parameters</u>"),
                   selectInput("summarize_by", "Summarize By:",
                               choices = c("Events", "Patients")),
                   selectInput("color_by", "Color By:",
                               choices = c("None", "Sex", "Race", "Severity", "Outcome", "Seriousness", "Relatedness"),
                               selected = "None"),
                   HTML("<u>Choose Filters</u>"),
                   selectInput("severity", "Severity:", 
                               choices = c("All", "Mild", "Moderate", "Severe")),
                   selectInput("outcome", "Outcome:",
                               choices = c("All", capitalize_first(tolower(unique(ae$AEOUT))))),
                   selectInput("serious", "Serious?:",
                               choices = c("All", "Yes", "No")),
                   selectInput("related", "Related to Treatment?",
                               choices = c("All", "Not Related", "Unlikely Related", "Possibly Related",
                                           "Probably Related", "Definitely Related")),
                   numericInputIcon("prevalence", "Prevalence", 
                                    value = 0,
                                    icon = list(icon("greater-than-equal"), icon("percent")))),
                 fluidRow(
                   column(6, plotlyOutput("plot2a",height = "600px")),
                   column(6, plotlyOutput("plot2b", height = "520px"))
                 ),
                 absolutePanel(
                   top = 10, right = 10,  # Position the dropdown at the top-right corner
                   width = 120,
                   style = "z-index: 1000; background-color: white; padding: 10px;",
                   selectInput(
                     "site_filter2", "Site:",
                     choices = c("All", "Site 01", "Site 02", "Site 03", "Site 04", "Site 05")
                   )
                 ),
                 h4("Details-On-Demand", align = "center"),
                 DT:::dataTableOutput("drill_down_table_2")
                 
               )
              
      ),
      tabPanel("Adverse Events Timeline")
    )
  ),
  
  ########## Clinical Timelines
  nav_panel(
    title = "Clinical Timelines"
  ),
  ########## Safety
  nav_panel(
    title = "Safety",
    tabsetPanel(
      tabPanel("Safety Histogram"),
      tabPanel("Safety Outlier Explorer"),
      tabPanel("Results Over Time")
    )
  )
)


# Look into shinydashboard package. Has some cool themes.
# Dashboard$page, dashboard$sidebar(), etc.
