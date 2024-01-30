#########################################################################
# Name of file - 00_setup.R
# Health and Wellbeing (HWB) Census 
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 4.2.2
#
# Description - Sets up environment required for running HWB RAP. 
# This is the only file which requires manual changes before the 
# RAP process is run.
#########################################################################


### 1 - Load packages ----

library(here)
library(readr)
library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(cli)
library(usethis)
library(purrr)
library(DBI)
library(openxlsx)
library(RtoSQLServer)
library(glue)



### 2 - Load functions (from functions folder) ----

source(here("functions", "check_headers.R"))
source(here("functions", "clean_strings.R"))
source(here("functions", "new_folders.R"))
source(here("functions", "new_folders_merged.R"))
source(here("functions", "new_output_folders.R"))



### 3 - Set file path for raw data and output folders ----

raw_data_folder <- "//s0196a/ADM-Education-NIF Analysis/Health and Wellbeing Survey/R/RAP Project/raw_data"

output_folder <- here("output")



### 4 - Set parameters ----

year <- 2022

pupil_census_year <- 2021

all_las <- c(
  "Angus", "Clackmannanshire", "Dumfries & Galloway", "Dundee",
  "East Renfrewshire", "Edinburgh City", "Falkirk", "Glasgow",
  "Moray", "North Ayrshire", "Perth & Kinross", "Renfrewshire",
  "Scottish Borders", "Shetland", "South Ayrshire", "Stirling"
)

all_stages <- c(paste0("P", 5:7), paste0("S", 1:6))

# Define expected pattern for question numbers in header
# Accepted patterns: Q1. Q1.1. Q11.1 Q1.11 Q11.11
q_pattern <- "Q\\d{1,2}\\.(\\d{1,2}\\.)?"

pupil_census_year <- 2021

studentnaturesupport_year <- 2021



### 5 - Create output folder ----

use_directory(paste0("output/", year))



### 6 - Run new_folders function to create new folders for each LA in raw_data_folder

for (la in all_las){
  new_folders(year, la, raw_data_folder)
}



### 7 - Run new_folders_merged function to create new folder for merged data

new_folders_merged(year, raw_data_folder)



### 8 - Run new_output_folders function to create new folders for each LA and for National in output_folder 

for (la in all_las){
  new_output_folders(year, la, output_folder)
}

new_output_folders(year, "National", output_folder)


### END OF SCRIPT ###
