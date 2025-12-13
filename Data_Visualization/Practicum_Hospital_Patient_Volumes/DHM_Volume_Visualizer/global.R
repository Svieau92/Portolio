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
sheets <- c("AY21", "AY22", "AY23", "AY24", "AY25", "AY26")

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
  mutate(`HMS 9` = coalesce(`HMS 9`, `HMS 9\r\nTeam 1`),
         `HMS 10` = coalesce(`HMS 10`, `HMS 10\r\nTeam 1`),
         `HMS 11` = coalesce(`HMS 11`, `HMS 11\r\nTeam 1`),
         `HMS 12` = coalesce(`HMS 12`, `HMS 12\r\nTeam 2`),
         HMS15Floor = coalesce(`HMS 15`, `HMS 15\r\nFloor`), # Combine HMS 15 groups with different names from different years
         `Medicine \r\nConsults` = coalesce(CON, `Medicine \r\nConsults`) # Combine Med Consults groups with different names from different years
  ) |> 
  rename(
    Htransplant = `H. Transplant\r\nTeam 2`, # Rename Transplant group to match name in daily caps sheet
    HMS15Stepdown = `HMS 15\r\nStepdown` # Rename HMS15 stepdown group to match name in daily caps sheet
  ) |> 
  select(-`HMS 9\r\nTeam 1`, -`HMS 10\r\nTeam 1`, -`HMS 11\r\nTeam 1`, 
         -`HMS 12\r\nTeam 2`, -`HMS 15`, -`HMS 15\r\nFloor`, -CON,
         -`HOSP A`, -`Surge ICU 7`)

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
    cols = c(HMS1:IMTOTAL),  # adjust as needed
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
      str_detect(Team, "^ACE") ~ "ACE",
      Team %in% c("AddictionMed", "Addiction\r\nMed") ~ "Addiction Medicine",
      Team %in% c("HMS1", "HMS2", "HMS3", "HMS4", "HMS5", "HMS6", "HMS8", "HMS9", "HMS10", "HMS11", "HMS12", "HMS13", "HMS14", "HMS15Floor", "HMS16", "HMS17", "HMS18", "HMS19", "HMS20", "HTT", "MED1", "MED2", "MED3", "MED4") ~ "General Medicine",
      Team == "HMS15Stepdown" ~ "Stepdown",
      Team == "HMS7" ~ "HMS7",
      str_detect(Team, "^ONC") ~ "Med Oncology",
      Team %in% c("Medicine\r\nConsults", "NEWMED\r\nCONSULTS") ~ "Medicine Consults",
      Team == "Htransplant" ~ "Transplant",
      Team == "DAY" ~ "Day",
      Team == "NIGHT" ~ "Night",
      Team == "SWING" ~ "Swing",
      TRUE ~ Team
    )
  )

# Combine teams that are considered to be one group
census_with_capacity <- census_with_capacity |>
  mutate(
    Team = case_when(
      Team %in% c("HMS9", "HMS10", "HMS11") ~ "Team 1",
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

# ⚠️ HARD-CODED CAPACITIES ⚠️
# Manually set capacities based on Dimitriy's reccomendations
census_with_capacity <- census_with_capacity |>
  mutate(Capacity = case_when(
    ServiceLine == "Addiction Medicine" ~ 16,
    ServiceLine == "Stepdown" ~ 14,
    ServiceLine == "Medicine Consults" ~ 48,
    ServiceLine == "Swing" ~ 19,
    ServiceLine == "Day" ~ NA,
    ServiceLine == "Night" ~ NA,
    TRUE ~ Capacity
  ))

# Order service line for plotting
census_with_capacity <- census_with_capacity |>
  mutate(ServiceLine = factor(ServiceLine, levels =
                                c("ACE", "Addiction Medicine", "General Medicine", "Stepdown", "HMS7", "Med Oncology", "Medicine Consults", "Med Consults Combined", "Transplant", "Day", "Swing", "Night", "IMTOTAL", "AVAILCAP")))

# Define custom team order (This makes our cards for the Teams show in the desired order)
# Create a manual ordering variable
census_with_capacity <- census_with_capacity |>
  mutate(TeamOrder = case_when(
    Team == "HMS1" ~ 1,
    Team == "HMS2" ~ 2,
    Team == "HMS3" ~ 3,
    Team == "HMS4" ~ 4,
    Team == "HMS5" ~ 5,
    Team == "HMS6" ~ 6,
    Team == "HMS7" ~ 7,
    Team == "HMS8" ~ 8,
    Team == "Team 1" ~ 9,
    Team == "Team 2" ~ 10,
    Team == "HMS12" ~ 10.5,
    Team == "HMS13" ~ 11,
    Team == "HMS14" ~ 12,
    Team == "HMS15" ~ 13,
    Team == "HMS16" ~ 14,
    Team == "HMS17" ~ 15,
    Team == "HMS18" ~ 16,
    Team == "HMS19" ~ 17,
    Team == "HMS20" ~ 18,
    Team == "HTT" ~ 19,
    Team == "MED1" ~ 20,
    Team == "MED2" ~ 21,
    Team == "MED3" ~ 22,
    Team == "MED4" ~ 23,
    Team == "ACE1" ~ 24,
    Team == "ACE2" ~ 25,
    Team == "ACE3" ~ 26,
    Team == "Medicine\r\nConsults" ~ 27,
    Team == "NEWMED\r\nCONSULTS" ~ 28,
    Team == "Med Consults Combined" ~ 29,
    Team == "Addiction\r\nMed" ~ 30,
    Team == "ONC1" ~ 31,
    Team == "ONC2" ~ 32,
    Team == "ONC3" ~ 33,
    Team == "DAY" ~ 34,
    Team == "SWING" ~ 35,
    Team == "NIGHT" ~ 36,
    Team == "AVAILCAP" ~ 37,
    Team == "IMTOTAL" ~ 38,
    Team == "CON" ~ 39,
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
