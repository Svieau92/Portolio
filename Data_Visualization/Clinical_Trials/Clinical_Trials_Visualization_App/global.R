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
accrual <- read_csv("data/dashboard-accrual.csv")

# Load Subject Visits data set
sv <- read_csv("data/sv.csv")

# Load Demographics data set
dm <- read_csv("data/dm.csv")

# Load Adverse Events Data set
ae <- read_csv("data/ae.csv")

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
