#########################################################################
# Name of file - 13_formatting_and_suppression.R
# Health and Wellbeing (HWB) Census 
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 4.2.2
#
# Description - Formats and suppresses tables for publication
#########################################################################

### 0 - Setup ----

source(here::here("code", "00_setup.R"))
source(here::here("functions", "read_raw_data.R"))



### 1 - Read in raw data ----

# Define the path to Excel file
file_path <- file.path(raw_data_folder, year, "Merged", "09_joined_stages.xlsx")

# Read in dataframe
hwb_analysis <- readxl::read_xlsx(file_path, sheet = 1)

