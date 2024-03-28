#########################################################################
# Name of file - 12h_relationships_and_sexual_health.R
# Health and Wellbeing (HWB) Census 
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 4.2.2
#
# Description - Analyses data for relationships and sexual health
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

cat_order_1 <- c("Yes",
                 "No",
                 "Prefer not to say")

cat_order_2 <- c("Often", 
                 "Quite often", 
                 "Occasionally",
                 "Never",
                 "Prefer not to say")

cat_order_3 <- c("Fully agree", 
                 "Agree", 
                 "Disagree",
                 "Totally disagree",
                 "Prefer not to say",
                 "Doesn't apply to me")

cat_order_4 <- c("None", 
                 "Small amount (e.g. kissing, some intimate touching on top of clothes)", 
                 "Some experiences but no sexual intercourse (e.g. touching intimately underneath clothes or without clothes on)",
                 "More experiences, including oral sex",
                 "Vaginal or anal sex",
                 "Prefer not to say")

cat_order_5 <- c("Yes", 
                 "No",
                 "Don't know",
                 "Prefer not to say")



### 3 - Replace response values as per Measures for Inclusion in publication document ----

hwb_analysis$sex_experience_type[hwb_analysis$sex_experience_type == "More intimate experiences (not sexual intercourse)"] <- "Some experiences but no sexual intercourse (e.g. touching intimately underneath clothes or without clothes on)"
hwb_analysis$sex_experience_type[hwb_analysis$sex_experience_type == "Penetrative sex"] <- "Vaginal or anal sex"
hwb_analysis$sex_experience_type[hwb_analysis$sex_experience_type == "Sexual intercourse"] <- "Vaginal or anal sex"



### 4 - Define variables for analysis ----

variables <- data.frame(
  variable = c("sex_boyfriend_girlfriend",
               "sex_boyfriend_girlfriend_feel_safe",
               "sex_attitude_get_information",
               "sex_attitude_ask_for_help",
               "sex_attitude_saying_no",
               "sex_experience_type",
               "sex_last_time_condom_use",
               "sex_last_time_contraception_use"),
  cat_order = c("cat_order_1", 
                "cat_order_2", 
                "cat_order_3", 
                "cat_order_3", 
                "cat_order_3",
                "cat_order_4",
                "cat_order_5",
                "cat_order_5")
)



### 5 - Perform analysis on selected variables ----

# For national
national_sexual_health <- perform_analysis_national(hwb_analysis, variables)

# For each local authority except East Renfrewshire, Moray & Scottish Borders (as they did not ask any relationships and sexual health questions)
local_authority_list <- list()

for (value in all_las) {
  if (value %in% c("East Renfrewshire", "Moray", "Scottish Borders")) {
    cat("Skipping processing for:", value, "\n")
    next  # Move to the next iteration without executing the code below
  }
  
  cat("Processing local authority:", value, "\n")
  filtered_data <- hwb_analysis[hwb_analysis$pc_la == value, ]
  result <- perform_analysis_local_authority(filtered_data, variables)
  # Store the result with a dynamic name, e.g. Angus_sexual_health
  list_name <- paste0(value, "_sexual_health")
  local_authority_list[[list_name]] <- result
}



### 6 - Save combined_list as an excel file to Merged folder ----

# Save national
write_xlsx(
  national_sexual_health,
  here("output", year, "National", "Output", paste0(year, "_relationships_and_sexual_health.xlsx"))
)

# Save local authorities
# Function to save tibbles in respective folders
save_tibbles_as_sheets <- function(tibble_list, folder_name) {
  file_path <- here::here("output", year, folder_name, "Output", paste0(year, "_relationships_and_sexual_health.xlsx"))
  write_xlsx(
    tibble_list,
    path = file_path,
    col_names = TRUE
  )
}

# Save each list of tibbles as a single Excel file with multiple sheets
map2(local_authority_list, all_las[!all_las %in% c("East Renfrewshire", "Moray", "Scottish Borders")], save_tibbles_as_sheets)



### END OF SCRIPT ###

