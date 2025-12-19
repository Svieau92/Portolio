#######################################################################################################################
# App Name: Division of Hospital Medicine Patient Volume Visualizer                                                   #
# Author: Sean Vieau                                                                                                  #
# Date Created: 9/29/2025                                                                                             #
# Last Updated:                                                                                                       #
#                                                                                                                     #
# Info: This is the global.R file for the DHM Volume Visualizer. Here we load the libraries and data used in the app  #
#######################################################################################################################

#######################################################################################################################
#                                                   Load Libraries                                                    #
#######################################################################################################################

library(shiny)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(readxl)
library(lubridate)
library(purrr)
library(tibble)
library(fuzzyjoin) # For fuzzy join logic
library(data.table) # For Merging data sets
library(bslib) # For better ui options
library(forcats) # For reversing categories for plots
library(plotly) # For interactive plots
library(shinyWidgets) # For cooler icons
library(DT) # For outputting data tables
library(qicharts2) # For statistical process control plots
library(shinycssloaders) # For loading bar when user uploads data
library(patchwork) # For small multiples of spc plots
library(shinybusy) # For loading messages when processing data

#######################################################################################################################
#                                                     Load Data                                                       #
#######################################################################################################################

#==============================================================================================#
#                                 Read in Daily Census Data                                    #                    
#==============================================================================================#

# ⚠️ HARD-CODED EXCEL SHEETS⚠️
# Set sheets from the .xlsx file to be visualized
# (We can move this into the function later to make it a user selected option and reactive)
sheets <- c("AY21", "AY22", "AY23", "AY24", "AY25")

# Create function to transform uploaded data
process_uploaded_file <- function(vol_path, cap_path) {
  
# Set file path for census data based on user selected data
file_path <- vol_path

###### Create function to extract data set from each academic year sheet in the .xlsx file 
extract_AY_data <- function(file_path, sheet_name, end_col_name = "IM TOTAL") {
  
  # Read in the sheet
  raw_data <- read_xlsx(file_path, sheet = sheet_name, col_names = TRUE)
  
  # Remove the first column (it's extraneous)
  raw_data <- raw_data |> 
    select(-1)
  
  # Find the row index where the first column equals "Date"
  start_row <- which(raw_data[[1]] == "Date")[1]
  
  # Slice from that row onwards
  data_filter <- raw_data |> 
    slice(start_row:n())
  
  # Rename column names to be the first row, and remove that row
  colnames(data_filter) <- as.character(unlist(data_filter[1, ]))
  data_filter <- data_filter[-1, ]
  
  # Get the column index where the column name is "IM TOTAL" 
  # (This is the last column we are interested in including in the data)
  end_col <- match("IM TOTAL", colnames(data_filter))
  
  # Select all columns up to and including "IM TOTAL"
  data_filter <- data_filter |> 
    select(1:end_col)
  
  # Add Academic Year Variable
  data_filter <- data_filter |>
    mutate(AY = sheet_name) |> 
    relocate(AY, .after = 1)
  
  # Convert date values to date time, and filter out all rows that are not the daily values
  data_filter <- data_filter |> 
    mutate(Date = suppressWarnings(as.Date(as.numeric(Date), origin = "1899-12-30"))) |> 
    filter(!is.na(Date))
}

# Read and process each sheet into a named list
data_list <- map(sheets, ~ extract_AY_data(file_path, .x)) |> 
  set_names(sheets)

##### Extract Data Sets from Each Sheet

# Extract each data set with the naming scheme "data_AYxx" (E.g. data_AY21, data_AY22, ... data_AY25)
walk2(names(data_list), data_list, ~ assign(paste0("data_", .x), .y, envir = .GlobalEnv))

# Bind together all of our census datasets into a single data set
combined_data <- bind_rows(data_list, .id = "AY")

#==============================================================================================#
#                                 Read in Daily Capacities                                     #                    
#==============================================================================================#

# Read in DHM Team Caps data set
data_daily_caps <- read_xlsx(cap_path)

#######################################################################################################################
#                                                   Data Cleaning                                                     #
#######################################################################################################################

#==============================================================================================#
#                                      Consolidate Teams                                       #                    
#==============================================================================================#

# Combine "HMS 9 Team 1" and "HMS 9" columns into one (same for 10, 11, 12)
combined_data <- combined_data |>
  select(-`HOSP A`, -`Surge ICU 7`)

# Remove all excess spaces so team names match the format of the daily caps data set
combined_data <- combined_data |>
  rename_with(~ gsub(" ", "", .x))  # removes all spaces from column names

# Rearrange variable order
combined_data <- combined_data |> 
  relocate(DAY, SWING, NIGHT, IMTOTAL, .after = last_col())

# Filter out dates before granular data (1/1/2021), and after the current date 
combined_data <- combined_data |> 
  filter(Date >= as.Date("2021-01-01")) |> 
  filter(Date <= Sys.Date())                   # remove future dates

#==============================================================================================#
#                                   Convert to Long Form                                       #                    
#==============================================================================================#

# Pivot census data
census_long <- combined_data |>
  pivot_longer(
    cols = c('Team1':IMTOTAL),  # adjust as needed
    names_to = "Team",
    values_to = "Volume"
  )

# Pivot capacity data
capacity_long <- data_daily_caps |>
  pivot_longer(
    cols = ends_with("_cap"),
    names_to = "Team",
    values_to = "Capacity",
  ) |>
  mutate(Team = gsub("_cap$", "", Team))

#==============================================================================================#
#                                       Merge Data Sets                                        #                    
#==============================================================================================#

# Convert to data.table
census_dt <- as.data.table(census_long)
capacity_dt <- as.data.table(capacity_long)

# Ensure date columns are Date type
census_dt[, Date := as.Date(Date)]
capacity_dt[, StartDate := as.Date(StartDate)]
capacity_dt[, EndDate := as.Date(EndDate)]

# Set keys for fast non-equi join
setkey(capacity_dt, Team, StartDate, EndDate)

# Perform non-equi join
census_with_capacity <- capacity_dt[census_dt, 
                                    on = .(Team, StartDate <= Date, EndDate >= Date), 
                                    nomatch = NA,
                                    .(Date, Team, Volume, Capacity, totalcapacity_calc)]

# Convert Volume to numeric
census_with_capacity <- census_with_capacity |> 
  mutate(Volume = as.numeric(Volume))

# Create Month and Week values
census_with_capacity <- census_with_capacity |> 
  mutate(
    Week = floor_date(Date, unit = "week", week_start = 1), # Monday starts the week 
    Month = floor_date(Date, unit = "month")
  ) |> 
  select(Date, Week, Month, everything())

# ⚠️ HARD-CODED SERVICE LINES ⚠️
# Add service line level
census_with_capacity <- census_with_capacity |>
  mutate(
    ServiceLine = case_when(
      str_detect(Team, "^ACUTECARE") ~ "Acute Care",
      Team == "BehavioralHealth" ~ "Behavioral Health",
      Team %in% c("Team1","Team2","Team3","Team4","Team5","Team6",
                  "Team7","Team8","Team9","Team10",
                  "Team11","Team12","Team13", "Team14",
                  "Team15A", "HospitalTeam",
                  "MedTeam1","MedTeam2","MedTeam3","MedTeam4") ~ "Medical Ward",
      Team == "Team15B" ~ "Intermediate Care",
      str_detect(Team, "^CancerCare") ~ "Cancer Care",
      str_detect(Team, "^ConsultTeam") ~ "Consult Service",
      Team == "DAY" ~ "Day Shift",
      Team == "SWING" ~ "Evening Shift",
      Team == "NIGHT" ~ "Night Shift",
      TRUE ~ Team
    )
  )

# Combine teams that are considered to be one group
census_with_capacity <- census_with_capacity |>
  mutate(
    Team = case_when(
      Team %in% c("Team9", "Team10", "Team11") ~ "TeamA",
      # Team %in% c("HMS12", "Htransplant") ~ "Team 2",
      TRUE ~ Team
    )
  ) |>
  group_by(Date, Week, Month, Team, ServiceLine) |>
  summarise(
    Volume = sum(Volume, na.rm = TRUE),
    Capacity = sum(Capacity, na.rm = TRUE),
    totalcapacity_calc = first(totalcapacity_calc),  # assuming it's constant
    .groups = "drop"
  )

# ⚠️ UPDATED CAPACITIES ⚠️
# Manually set capacities based on recommendations
census_with_capacity <- census_with_capacity |>
  mutate(Capacity = case_when(
    ServiceLine == "Behavioral Health" ~ 16,
    ServiceLine == "Intermediate Care" ~ 14,
    ServiceLine == "Consult Service" ~ 48,
    ServiceLine == "Evening Shift" ~ 19,
    ServiceLine == "Day Shift" ~ NA,
    ServiceLine == "Night Shift" ~ NA,
    TRUE ~ Capacity
  ))

# Order service line for plotting
census_with_capacity <- census_with_capacity |>
  mutate(ServiceLine = factor(ServiceLine, levels = c(
    "Acute Care",
    "Behavioral Health",
    "Medical Ward",
    "Intermediate Care",
    "Cancer Care",
    "Consult Service",
    "Hospital Service",
    "Day Shift",
    "Evening Shift",
    "Night Shift"
  )))

# Define custom team order (for card display)
census_with_capacity <- census_with_capacity |>
  mutate(TeamOrder = case_when(
    Team == "Team1" ~ 1,
    Team == "Team2" ~ 2,
    Team == "Team3" ~ 3,
    Team == "Team4" ~ 4,
    Team == "Team5" ~ 5,
    Team == "Team6" ~ 6,
    Team == "Team7" ~ 7,
    Team == "Team8" ~ 8,
    Team == "Team9" ~ 9,
    Team == "Team10" ~ 10,
    Team == "Team11" ~ 11,
    Team == "Team12" ~ 12,
    Team == "Team13" ~ 13,
    Team == "Team14" ~ 14,
    Team == "Team15A" ~ 15,
    Team == "Team15B" ~ 16,
    Team == "HospitalTeam" ~ 17,
    Team == "MedTeam1" ~ 18,
    Team == "MedTeam2" ~ 19,
    Team == "MedTeam3" ~ 20,
    Team == "MedTeam4" ~ 21,
    Team == "ACUTECARE1" ~ 22,
    Team == "ACUTECARE2" ~ 23,
    Team == "ACUTECARE3" ~ 24,
    Team == "ConsultTeamA" ~ 25,
    Team == "ConsultTeamB" ~ 26,
    Team == "BehavioralHealth" ~ 27,
    Team == "CancerCare1" ~ 28,
    Team == "CancerCare2" ~ 29,
    Team == "CancerCare3" ~ 30,
    Team == "DAY" ~ 31,
    Team == "SWING" ~ 32,
    Team == "NIGHT" ~ 33,
    TRUE ~ NA_real_
  ))

# Sort
census_with_capacity <- census_with_capacity |>
  arrange(Date, TeamOrder)

# Strip extraneous characters
census_with_capacity <- census_with_capacity %>%
  mutate(
    Team = str_replace_all(Team, "[\\r\\n]", ""),  # remove carriage returns/newlines
    Team = str_trim(Team)                          # trim spaces
  )

# Return final data sets
return(list(
  census_with_capacity = census_with_capacity,
  combined_data = combined_data,
  data_daily_caps = data_daily_caps))
}

#######################################################################################################################
#                                                       End                                                           #
#######################################################################################################################

# Automatically load data sets for RShiny website hosting
default_result <- process_uploaded_file("data/census_data.xlsx", "data/team_caps_data.xlsx")

census_with_capacity <- reactiveVal(default_result$census_with_capacity)
combined_data        <- reactiveVal(default_result$combined_data)
data_daily_caps      <- reactiveVal(default_result$data_daily_caps)



