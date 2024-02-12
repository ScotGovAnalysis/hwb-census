#########################################################################
# Name of file - 12c_mental_health_and_wellbeing.R
# Health and Wellbeing (HWB) Census 
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 4.2.2
#
# Description - Analyses data for mental health and wellbeing
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

cat_order_1 <- c("Strongly agree or Agree",
                 "Neither agree nor disagree",
                 "Strongly disagree or Disagree",
                 "Prefer not to say")

cat_order_2 <- c("Agree",
                 "Neither agree nor disagree",
                 "Disagree",
                 "Don't know")

cat_order_3 <- c("Often or All of the time",
                 "Some of the time",
                 "Rarely or None of the time")

cat_order_4 <- c("Often or Always",
                 "Sometimes",
                 "Hardly ever or Never")

cat_order_5 <- c("None at all",
                 "Up to 2 hours",
                 "3 or more hours",
                 "3-5 hours",
                 "6 or more hours")

cat_order_6 <- c("Problematic use",
                 "Non-problematic use")



### 3 - Replace response values as per Measures for Inclusion in publication document ----

hwb_analysis[hwb_analysis == "Strongly agree"] <- "Strongly agree or Agree"
hwb_analysis[hwb_analysis == "Agree"] <- "Strongly agree or Agree"
hwb_analysis[hwb_analysis == "Strongly disagree"] <- "Strongly disagree or Disagree"
hwb_analysis[hwb_analysis == "Disagree"] <- "Strongly disagree or Disagree"

hwb_analysis$frequency_feeling_confident[hwb_analysis$frequency_feeling_confident == "Often"] <- "Often or All of the time"
hwb_analysis$frequency_feeling_confident[hwb_analysis$frequency_feeling_confident == "All of the time"] <- "Often or All of the time"
hwb_analysis$frequency_feeling_confident[hwb_analysis$frequency_feeling_confident == "Rarely"] <- "Rarely or None of the time"
hwb_analysis$frequency_feeling_confident[hwb_analysis$frequency_feeling_confident == "None of the time"] <- "Rarely or None of the time"

hwb_analysis$frequency_feeling_lonely[hwb_analysis$frequency_feeling_lonely == "Often"] <- "Often or Always"
hwb_analysis$frequency_feeling_lonely[hwb_analysis$frequency_feeling_lonely == "Often or always"] <- "Often or Always"
hwb_analysis$frequency_feeling_lonely[hwb_analysis$frequency_feeling_lonely == "Some of the time"] <- "Sometimes"
hwb_analysis$frequency_feeling_lonely[hwb_analysis$frequency_feeling_lonely == "Hardly ever or never"] <- "Hardly ever or Never"

hwb_analysis$frequency_feeling_left_out[hwb_analysis$frequency_feeling_left_out == "Often or always"] <- "Often or Always"
hwb_analysis$frequency_feeling_left_out[hwb_analysis$frequency_feeling_left_out == "Hardly ever or never"] <- "Hardly ever or Never"

hwb_analysis[hwb_analysis == "Some time (up to 2 hours a day)"] <- "Up to 2 hours"
hwb_analysis[hwb_analysis == "Some time (up to about 2 hours a day)"] <- "Up to 2 hours"
hwb_analysis[hwb_analysis == "About half an hour"] <- "Up to 2 hours"
hwb_analysis[hwb_analysis == "About 1 hour a day"] <- "Up to 2 hours"
hwb_analysis[hwb_analysis == "About 2 hours a day"] <- "Up to 2 hours"
hwb_analysis[hwb_analysis == "Quite a bit of time (about 3 hours a day or more)"] <- "3 or more hours"
hwb_analysis[hwb_analysis == "About 3 hours a day"] <- "3-5 hours"
hwb_analysis[hwb_analysis == "About 4 hours a day"] <- "3-5 hours"
hwb_analysis[hwb_analysis == "About 5 hours a day"] <- "3-5 hours"
hwb_analysis[hwb_analysis == "About 6 hours a day"] <- "6 or more hours"
hwb_analysis[hwb_analysis == "About 7 hours or more a day"] <- "6 or more hours"



### 4 - Split the questions time_social_media_weekdays and time_social_media_weekends into primary and secondary ----
# This is because those questions had different response options for primary and secondary pupils so they are not directly comparable

hwb_analysis <- hwb_analysis %>%
  mutate(
    # Creating a new column time_social_media_weekdays_primary
    # Checking if the first character of pc_stage is "S"
    # If true, assign "Question not asked of stage", else keep the original value
    time_social_media_weekdays_primary = ifelse(substr(pc_stage, 1, 1) == "S", "Question not asked of stage", time_social_media_weekdays),
    time_social_media_weekdays_secondary = ifelse(substr(pc_stage, 1, 1) == "P", "Question not asked of stage", time_social_media_weekdays),
    time_social_media_weekends_primary = ifelse(substr(pc_stage, 1, 1) == "S", "Question not asked of stage", time_social_media_weekends),
    time_social_media_weekends_secondary = ifelse(substr(pc_stage, 1, 1) == "P", "Question not asked of stage", time_social_media_weekends)
  )



### 5 - Create derived variable for social media disorder ----

# Function to determine social media disorder based on row values
get_social_media_disorder <- function(row) {
  row_values <- unlist(row)
  
  # Rule 1: If all columns have "Question not asked of stage"
  if(all(row_values == "Question not asked of stage" | is.na(row_values))) {
    return("Question not asked of stage")
  }
  
  # Rule 2: If all columns have "Data not collected"
  if(all(row_values == "Data not collected" | is.na(row_values))) {
    return("Data not collected")
  }

  # Rule 3: If all columns have a combination of "Yes" and "No" with at least six "Yes"
  if(all(row_values %in% c("Yes", "No")) &&
    sum(row_values == "Yes", na.rm = TRUE) >= 6) {
  return("Problematic")
}

  # Rule 4: If all columns have a combination of "Yes" and "No" with five or fewer "Yes"
  if(all(row_values %in% c("Yes", "No")) &&
     sum(row_values == "Yes", na.rm = TRUE) <= 5) {
  return("Non-problematic")
}

  # Rule 5: Else NA
  return(NA)
}

# Apply the function row-wise to create the new column
hwb_analysis$social_media_disorder <- apply(hwb_analysis[, grepl("^social_media_impact", names(hwb_analysis))], 1, get_social_media_disorder)



### 6 - Define variables for analysis ----

variables <- data.frame(
  variable = c("life_is_just_right",
               "wish_for_different_life",
               "have_what_I_want",
               "adults_listening",
               "adults_taking_into_account",
               "like_who_i_am",
               "proud_of_things",
               "try_my_hardest",
               "can_make_decisions",
               "generally_cheerful",
               "lots_of_worries",
               "I_will_be_ok",
               "frequency_feeling_confident",
               "frequency_feeling_lonely",
               "frequency_feeling_left_out",
               "happy_with_my_body",
               "my_looks_affect_how_I_feel",
               "time_social_media_weekdays_primary", 
               "time_social_media_weekdays_secondary", 
               "time_social_media_weekends_primary",
               "time_social_media_weekends_secondary",
               "social_media_disorder"),
  cat_order = c("cat_order_1", 
                "cat_order_1", 
                "cat_order_1", 
                "cat_order_2", 
                "cat_order_2",
                "cat_order_1",
                "cat_order_2",
                "cat_order_1",
                "cat_order_1",
                "cat_order_1",
                "cat_order_1",
                "cat_order_1",
                "cat_order_3",
                "cat_order_4",
                "cat_order_4",
                "cat_order_1",
                "cat_order_1",
                "cat_order_5",
                "cat_order_5",
                "cat_order_5",
                "cat_order_5",
                "cat_order_6")
)



### 7 - Perform analysis on selected variables ----

# For national
national_mental_health <- perform_analysis_national(hwb_analysis, one_characteristics, stage_and_characteristics, variables)

# For each local authority
local_authority_list <- list()

for (value in all_las) {
  cat("Processing local authority:", value, "\n")
  filtered_data <- hwb_analysis[hwb_analysis$pc_la == value, ]
  result <- perform_analysis_local_authority(filtered_data, one_characteristics, stage_and_characteristics, variables)
  # Store the result with a dynamic name, e.g. Angus_mental_health
  list_name <- paste0(value, "_mental_health")
  local_authority_list[[list_name]] <- result
}



### 8 - Save combined_list as an excel file to Merged folder ----

# Save national
write_xlsx(
  national_mental_health,
  here("output", year, "National", paste0(year, "_mental_health_and_wellbeing.xlsx"))
)

# Save local authorities
# Function to save tibbles in respective folders
save_tibbles_as_sheets <- function(tibble_list, folder_name) {
  file_path <- here::here("output", year, folder_name, paste0(year, "_mental_health_and_wellbeing.xlsx"))
  write_xlsx(
    tibble_list,
    path = file_path,
    col_names = TRUE
  )
}

# Save each list of tibbles as a single Excel file with multiple sheets
map2(local_authority_list, all_las, save_tibbles_as_sheets)



### END OF SCRIPT ###

