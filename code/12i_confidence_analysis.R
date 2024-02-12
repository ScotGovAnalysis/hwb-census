#########################################################################
# Name of file - 12l_confidence_analysis.R
# Health and Wellbeing (HWB) Census 
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 4.2.2
#
# Description - Analyses data for confidence questions
#########################################################################

### 0 - Setup ----

source(here::here("code", "00_setup.R"))
source(here::here("functions", "read_raw_data.R"))
source(here::here("functions", "analysis_one_characteristic.R"))
source(here::here("functions", "analysis_stage_and_characteristic.R"))
source(here::here("functions", "perform_analysis_national.R"))
source(here::here("functions", "perform_analysis_local_authority.R"))



### 1 - Read in raw data ----

hwb_analysis <- read_xlsx(file.path(raw_data_folder, year, "Merged", "09_joined_stages.xlsx"), sheet = 1)



### 2 - Set row order of response categories ----

cat_order_1 <- c("Often or All of the time",
                 "Some of the time",
                 "Rarely or None of the time")



### 3 - Replace response values as per Measures for Inclusion in publication document ----

# Combine columns wemwbs_10_confident and frequency_feeling_confident into one column called 'How often have you been feeling confident?'
# This is because the question was asked differently of different stages (frequency_feeling_confident P5-S1 and wemwbs_10_confident S2-S6)

hwb_analysis$'How often have you been feeling confident?' <- ifelse(hwb_analysis$pc_stage %in% c("P5", "P6", "P7", "S1"),
                                 hwb_analysis$frequency_feeling_confident,
                                 hwb_analysis$wemwbs_10_confident)

hwb_analysis$'How often have you been feeling confident?'[hwb_analysis$'How often have you been feeling confident?' == "Often"] <- "Often or All of the time"
hwb_analysis$'How often have you been feeling confident?'[hwb_analysis$'How often have you been feeling confident?' == "All of the time"] <- "Often or All of the time"
hwb_analysis$'How often have you been feeling confident?'[hwb_analysis$'How often have you been feeling confident?' == "Rarely"] <- "Rarely or None of the time"
hwb_analysis$'How often have you been feeling confident?'[hwb_analysis$'How often have you been feeling confident?' == "None of the time"] <- "Rarely or None of the time"



### 4 - Define variables for analysis ----

variables <- data.frame(
  variable = c("How often have you been feeling confident?"),
  cat_order = c("cat_order_1")
)



### 5 - Perform analysis on selected variables ----

# For national
national_confidence_analysis <- perform_analysis_national(hwb_analysis, one_characteristics, stage_and_characteristics, variables)

# For each local authority
local_authority_list <- list()

for (value in all_las) {
  cat("Processing local authority:", value, "\n")
  filtered_data <- hwb_analysis[hwb_analysis$pc_la == value, ]
  result <- perform_analysis_local_authority(filtered_data, one_characteristics, stage_and_characteristics, variables)
  # Store the result with a dynamic name, e.g. Angus_confidence_analysis
  list_name <- paste0(value, "_confidence_analysis")
  local_authority_list[[list_name]] <- result
}



### 6 - Save combined_list as an excel file to Merged folder ----

# Save national
write_xlsx(
  national_confidence_analysis,
  here("output", year, "National", paste0(year, "_confidence_analysis.xlsx"))
)

# Save local authorities
# Function to save tibbles in respective folders
save_tibbles_as_sheets <- function(tibble_list, folder_name) {
  file_path <- here::here("output", year, folder_name, paste0(year, "_confidence_analysis.xlsx"))
  write_xlsx(
    tibble_list,
    path = file_path,
    col_names = TRUE
  )
}

# Save each list of tibbles as a single Excel file with multiple sheets
map2(local_authority_list, all_las, save_tibbles_as_sheets)



### END OF SCRIPT ###

