#########################################################################
# Name of file - 12e_mental_health_and_wellbeing_wemwbs.R
# Health and Wellbeing (HWB) Census 
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 4.2.2
#
# Description - Analyses data for mental health and wellbeing WEMWBS
#########################################################################

### 0 - Setup ----

source(here::here("code", "00_setup.R"))
source(here::here("functions", "read_raw_data.R"))
source(here::here("functions", "analysis_wemwbs_one_characteristic.R"))
source(here::here("functions", "analysis_wemwbs_stage_and_characteristic.R"))
source(here::here("functions", "perform_analysis_national_wemwbs.R"))
source(here::here("functions", "perform_analysis_local_authority_wemwbs.R"))



### 1 - Read in raw data ----

hwb_analysis <- read_xlsx(file.path(raw_data_folder, year, "Merged", "09_joined_stages.xlsx"), sheet = 1)



### 2 - Create derived variable for average WEMWBS score ----

# Identify columns that start with "wemwbs"
wemwbs_columns <- grep("^wemwbs", names(hwb_analysis), value = TRUE)

# Replace values in WEMWBS columns with numeric scores based on conditions
hwb_analysis <- hwb_analysis %>%
  mutate(across(all_of(wemwbs_columns), ~case_when(
    . == "None of the time" ~ 1,
    . == "Rarely" ~ 2,
    . == "Some of the time" ~ 3,
    . == "Often" ~ 4,
    . == "All of the time" ~ 5,
    TRUE ~ NA_real_
  )))

# Convert columns to numeric
hwb_analysis[wemwbs_columns] <- lapply(hwb_analysis[wemwbs_columns], as.numeric)

# Calculate total WEMWBS score (if any of the WEMWBS columns are NA then total_wemwbs is NA)
hwb_analysis$total_wemwbs <- ifelse(
  rowSums(is.na(hwb_analysis[, wemwbs_columns])) > 0, 
  NA, 
  rowSums(hwb_analysis[, wemwbs_columns], na.rm = TRUE)
)



### 3 - Define variables for analysis ----

variables <- data.frame(
  variable = c("total_wemwbs"))



### 4 - Perform analysis on selected variables ----

# For national
national_wemwbs <- perform_analysis_national_wemwbs(hwb_analysis, variables)

# For each local authority
local_authority_list <- list()

for (value in all_las) {
  cat("Processing local authority:", value, "\n")
  filtered_data <- hwb_analysis[hwb_analysis$pc_la == value, ]
  result <- perform_analysis_local_authority_wemwbs(filtered_data, variables)
  # Store the result with a dynamic name, e.g. Angus_wemwbs
  list_name <- paste0(value, "_wemwbs")
  local_authority_list[[list_name]] <- result
}



### 5 - Convert tibbles to character datatype from double ----

# Function to convert columns to character data type
convert_to_character <- function(df) {
  df %>% mutate(across(everything(), as.character))
}

# Convert columns to character in national_wemwbs
national_wemwbs <- map(national_wemwbs, ~ convert_to_character(.))

# Convert columns to character in local_authority_list
local_authority_list <- map(local_authority_list, ~ map(., convert_to_character))



### 6 - Save outputs as an excel file to Merged folder ----

# Save national
write_xlsx(
  national_wemwbs,
  here("output", year, "National", "Output", paste0(year, "_mental_health_and_wellbeing_wemwbs.xlsx"))
)

# Save local authorities
# Function to save tibbles in respective folders
save_tibbles_as_sheets <- function(tibble_list, folder_name) {
  file_path <- here::here("output", year, folder_name, "Output", paste0(year, "_mental_health_and_wellbeing_wemwbs.xlsx"))
  write_xlsx(
    tibble_list,
    path = file_path,
    col_names = TRUE
  )
}

# Save each list of tibbles as a single Excel file with multiple sheets
map2(local_authority_list, all_las, save_tibbles_as_sheets)


### END OF SCRIPT ###


