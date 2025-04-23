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
        
        ########## Accrual
        card(
          card_header(
            div(style = "display: flex; justify-content: space-between; align-items: center;",
                h4("Accrual"),
                selectInput("arm_filter1a", "Arm:", 
                            choices = c("All", unique(accrual$`filter:Arm`)), 
                            selected = "All",
                            width = "150px")
            )
          ),
          card_body(
            plotlyOutput("plot1a")
          )
        ),
        
        ########## Accrual Over Time
        card(
          card_header(
            div(style = "display: flex; justify-content: space-between; align-items: center;",
                h4("Accrual Over Time"),
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
        
        ########## Visit Completion
        card(
          card_header(
            div(style = "display: flex; justify-content: space-between; align-items: center;",
                h4("Visit Completion"),
                div(style = "margin-left: auto",
                selectInput("site_filter1c", "Site:",  
                            choices = c("All", "Site 01", "Site 02", "Site 03", "Site 04", "Site 05"), 
                            selected = "All",
                            width = "150px")),
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
      layout_columns(
        
        ########## Actual Vs Target Screened
        card(card_header("Actual vs. Target Subjects Screened"), plotlyOutput("plot1d")),
        
        ########## Actual Vs Target Randomized
        card(card_header("Actual vs. Target Subjects Randomized"), plotlyOutput("plot1e")),
        
        ########## Completions and Discontinuations
        card(card_header("Completions and Discontinuations"), plotlyOutput("plot1f"))
      )
    )
  ),
  
  ##############################################################################################
  ########### ADVERSE EVENTS ############
  
  ##### Adverse Events Summary
  nav_panel(
    title = "Adverse Events",
    tabsetPanel(
      tabPanel("Adverse Events Summary",
               layout_sidebar(
                 sidebar = sidebar(
                   bg = "lightgrey",
                   selectInput("summarize_by", "Summarize By:",
                               choices = c("Participants", "Events")),
                   selectInput("color_by", "Color By:",
                               choices = c("None", "Sex", "Race", "Severity", "Outcome", "Seriousness", "Relatedness"),
                               selected = "None"),
                   selectInput("severity", "Severity:", 
                               choices = c("All", "Mild", "Moderate", "Severe")),
                   selectInput("outcome", "Outcome:",
                               choices = c("All", capitalize_first(tolower(unique(ae$AEOUT))))),
                   selectInput("site_filter2", "Site:",  
                               choices = c("All", "Site 01", "Site 02", "Site 03", "Site 04", "Site 05")),
                   selectInput("serious", "Serious?:",
                               choices = c("All", "Yes", "No")),
                   selectInput("related", "Related to Treatment?",
                               choices = c("All", "Not Related", "Unlikely Related", "Possibly Related",
                                           "Probably Related", "Definitely Related"))),
                 plotOutput("plot2a")
               ),
      ),
      tabPanel("Adverse Events by Site"),
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
