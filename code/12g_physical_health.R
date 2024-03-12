#########################################################################
# Name of file - 12g_physical_health.R
# Health and Wellbeing (HWB) Census 
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 4.2.2
#
# Description - Analyses data for physical health
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

cat_order_1 <- c("Good or excellent",
                 "Poor or fair",
                 "Prefer not to say")

cat_order_2 <- c("Never",
                 "Between one or four days",
                 "Every day",
                 "Prefer not to say")

cat_order_3 <- c("Never or less than once a day",
                 "At least once a day")

cat_order_4 <- c("Never",
                 "Less than once a day",
                 "At least once a day")

cat_order_5 <- c("Always or often",
                 "Sometimes",
                 "Never",
                 "Prefer not to say")

cat_order_6 <- c("None or less than 1 hour",
                 "1 to 2 hours",
                 "2 hours or more",
                 "Prefer not to say")

cat_order_7 <- c("Every day", 
                 "At least once a week but not every day",
                 "Less than once a week (once a month or less than once a month)",
                 "Never",
                 "Prefer not to say")

cat_order_8 <- c("6 hours or less",
                 "Between 6 - 7 hours",
                 "Between 7 - 8 hours",
                 "Between 8 - 9 hours",
                 "Between 9 - 10 hours",
                 "More than 10 hours")



### 3 - Replace response values as per Measures for Inclusion in publication document ----

hwb_analysis$general_health[hwb_analysis$general_health == "Excellent"] <- "Good or excellent"
hwb_analysis$general_health[hwb_analysis$general_health == "Good"] <- "Good or excellent"
hwb_analysis$general_health[hwb_analysis$general_health == "Fair"] <- "Poor or fair"
hwb_analysis$general_health[hwb_analysis$general_health == "Poor"] <- "Poor or fair"

hwb_analysis$frequency_breakfast_weekday[hwb_analysis$frequency_breakfast_weekday == "I never have breakfast during weekdays"] <- "Never"
hwb_analysis$frequency_breakfast_weekday[hwb_analysis$frequency_breakfast_weekday == "One or two days"] <- "Between one or four days"
hwb_analysis$frequency_breakfast_weekday[hwb_analysis$frequency_breakfast_weekday == "Three or four days"] <- "Between one or four days"
hwb_analysis$frequency_breakfast_weekday[hwb_analysis$frequency_breakfast_weekday == "Every day"] <- "Every day"

hwb_analysis$frequency_soft_drinks[hwb_analysis$frequency_soft_drinks == "Never"] <- "Never or less than once a day"
hwb_analysis$frequency_soft_drinks[hwb_analysis$frequency_soft_drinks == "Once a week or less"] <- "Never or less than once a day"
hwb_analysis$frequency_soft_drinks[hwb_analysis$frequency_soft_drinks == "2-4 days a week"] <- "Never or less than once a day"
hwb_analysis$frequency_soft_drinks[hwb_analysis$frequency_soft_drinks == "5-6 days a week"] <- "Never or less than once a day"
hwb_analysis$frequency_soft_drinks[hwb_analysis$frequency_soft_drinks == "At least once a day"] <- "At least once a day"

hwb_analysis$frequency_chips[hwb_analysis$frequency_chips == "Never"] <- "Never or less than once a day"
hwb_analysis$frequency_chips[hwb_analysis$frequency_chips == "Once a week or less"] <- "Never or less than once a day"
hwb_analysis$frequency_chips[hwb_analysis$frequency_chips == "2-4 days a week"] <- "Never or less than once a day"
hwb_analysis$frequency_chips[hwb_analysis$frequency_chips == "5-6 days a week"] <- "Never or less than once a day"
hwb_analysis$frequency_chips[hwb_analysis$frequency_chips == "At least once a day"] <- "At least once a day"

hwb_analysis$frequency_crisps[hwb_analysis$frequency_crisps == "Never"] <- "Never or less than once a day"
hwb_analysis$frequency_crisps[hwb_analysis$frequency_crisps == "Once a week or less"] <- "Never or less than once a day"
hwb_analysis$frequency_crisps[hwb_analysis$frequency_crisps == "2-4 days a week"] <- "Never or less than once a day"
hwb_analysis$frequency_crisps[hwb_analysis$frequency_crisps == "5-6 days a week"] <- "Never or less than once a day"
hwb_analysis$frequency_crisps[hwb_analysis$frequency_crisps == "At least once a day"] <- "At least once a day"

hwb_analysis$frequency_sweets[hwb_analysis$frequency_sweets == "Never"] <- "Never or less than once a day"
hwb_analysis$frequency_sweets[hwb_analysis$frequency_sweets == "Once a week or less"] <- "Never or less than once a day"
hwb_analysis$frequency_sweets[hwb_analysis$frequency_sweets == "2-4 days a week"] <- "Never or less than once a day"
hwb_analysis$frequency_sweets[hwb_analysis$frequency_sweets == "5-6 days a week"] <- "Never or less than once a day"
hwb_analysis$frequency_sweets[hwb_analysis$frequency_sweets == "At least once a day"] <- "At least once a day"

hwb_analysis$frequency_fruit[hwb_analysis$frequency_fruit == "Once a week or less"] <- "Less than once a day"
hwb_analysis$frequency_fruit[hwb_analysis$frequency_fruit == "2-4 days a week"] <- "Less than once a day"
hwb_analysis$frequency_fruit[hwb_analysis$frequency_fruit == "5-6 days a week"] <- "Less than once a day"
hwb_analysis$frequency_fruit[hwb_analysis$frequency_fruit == "At least once a day"] <- "At least once a day"

hwb_analysis$frequency_vegetables[hwb_analysis$frequency_vegetables == "Once a week or less"] <- "Less than once a day"
hwb_analysis$frequency_vegetables[hwb_analysis$frequency_vegetables == "2-4 days a week"] <- "Less than once a day"
hwb_analysis$frequency_vegetables[hwb_analysis$frequency_vegetables == "5-6 days a week"] <- "Less than once a day"
hwb_analysis$frequency_vegetables[hwb_analysis$frequency_vegetables == "At least once a day"] <- "At least once a day"

hwb_analysis$frequency_being_hungry[hwb_analysis$frequency_being_hungry == "Always"] <- "Always or often"
hwb_analysis$frequency_being_hungry[hwb_analysis$frequency_being_hungry == "Often"] <- "Always or often"

hwb_analysis$time_physical_activity[hwb_analysis$time_physical_activity == "None"] <- "None or less than 1 hour"
hwb_analysis$time_physical_activity[hwb_analysis$time_physical_activity == "Less than half an hour"] <- "None or less than 1 hour"
hwb_analysis$time_physical_activity[hwb_analysis$time_physical_activity == "Between half an hour and 1 hour"] <- "None or less than 1 hour"

hwb_analysis$frequency_physical_activity[hwb_analysis$frequency_physical_activity == "4 to 6 times a week"] <- "At least once a week but not every day"
hwb_analysis$frequency_physical_activity[hwb_analysis$frequency_physical_activity == "2 to 3 times a week"] <- "At least once a week but not every day"
hwb_analysis$frequency_physical_activity[hwb_analysis$frequency_physical_activity == "Once a week"] <- "At least once a week but not every day"
hwb_analysis$frequency_physical_activity[hwb_analysis$frequency_physical_activity == "At least once a month but not every week"] <- "Less than once a week (once a month or less than once a month)"
hwb_analysis$frequency_physical_activity[hwb_analysis$frequency_physical_activity == "Once a month"] <- "Less than once a week (once a month or less than once a month)"
hwb_analysis$frequency_physical_activity[hwb_analysis$frequency_physical_activity == "Less than once a month"] <- "Less than once a week (once a month or less than once a month)"



### 4 - Create derived variable for mean hours of sleep on a school night ----

hwb_analysis <- hwb_analysis %>%
  mutate(avg_hours_sleep = case_when(
    time_bed == "Before 9.00 pm" & time_wake == "Before 5.00 am" ~ "Between 7 - 8 hours",    
    time_bed == "Before 9.00 pm" & time_wake == "At 5.00 am or later, but before 6.00 am" ~ "Between 8 - 9 hours", 
    time_bed == "Before 9.00 pm" & time_wake == "At 6.00 am or later, but before 7.00 am" ~ "Between 9 - 10 hours",
    time_bed == "Before 9.00 pm" & time_wake == "At 7.00 am or later, but before 8.00 am" ~ "More than 10 hours",
    time_bed == "Before 9.00 pm" & time_wake == "At 8.00 am or later" ~ "More than 10 hours",
    time_bed == "At 9.00 pm or later, but before 10.00 pm" & time_wake == "Before 5.00 am" ~ "Between 7 - 8 hours",    
    time_bed == "At 9.00 pm or later, but before 10.00 pm" & time_wake == "At 5.00 am or later, but before 6.00 am" ~ "Between 7 - 8 hours", 
    time_bed == "At 9.00 pm or later, but before 10.00 pm" & time_wake == "At 6.00 am or later, but before 7.00 am" ~ "Between 8 - 9 hours",
    time_bed == "At 9.00 pm or later, but before 10.00 pm" & time_wake == "At 7.00 am or later, but before 8.00 am" ~ "Between 9 - 10 hours",
    time_bed == "At 9.00 pm or later, but before 10.00 pm" & time_wake == "At 8.00 am or later" ~ "More than 10 hours",
    time_bed == "At 10.00 pm or later, but before 11.00 pm" & time_wake == "Before 5.00 am" ~ "Between 6 - 7 hours",    
    time_bed == "At 10.00 pm or later, but before 11.00 pm" & time_wake == "At 5.00 am or later, but before 6.00 am" ~ "Between 6 - 7 hours", 
    time_bed == "At 10.00 pm or later, but before 11.00 pm" & time_wake == "At 6.00 am or later, but before 7.00 am" ~ "Between 7 - 8 hours",
    time_bed == "At 10.00 pm or later, but before 11.00 pm" & time_wake == "At 7.00 am or later, but before 8.00 am" ~ "Between 8 - 9 hours",
    time_bed == "At 10.00 pm or later, but before 11.00 pm" & time_wake == "At 8.00 am or later" ~ "Between 9 - 10 hours",
    time_bed == "At 11.00 pm or later, but before midnight" & time_wake == "Before 5.00 am" ~ "6 hours or less",    
    time_bed == "At 11.00 pm or later, but before midnight" & time_wake == "At 5.00 am or later, but before 6.00 am" ~ "6 hours or less", 
    time_bed == "At 11.00 pm or later, but before midnight" & time_wake == "At 6.00 am or later, but before 7.00 am" ~ "Between 6 - 7 hours",
    time_bed == "At 11.00 pm or later, but before midnight" & time_wake == "At 7.00 am or later, but before 8.00 am" ~ "Between 7 - 8 hours",
    time_bed == "At 11.00 pm or later, but before midnight" & time_wake == "At 8.00 am or later" ~ "Between 8 - 9 hours",
    time_bed == "At midnight or later" & time_wake == "Before 5.00 am" ~ "6 hours or less",    
    time_bed == "At midnight or later" & time_wake == "At 5.00 am or later, but before 6.00 am" ~ "6 hours or less", 
    time_bed == "At midnight or later" & time_wake == "At 6.00 am or later, but before 7.00 am" ~ "Between 6 - 7 hours",
    time_bed == "At midnight or later" & time_wake == "At 7.00 am or later, but before 8.00 am" ~ "Between 7 - 8 hours",
    time_bed == "At midnight or later" & time_wake == "At 8.00 am or later" ~ "Between 7 - 8 hours",
    time_bed == "At midnight or later, but before 1.00 am" & time_wake == "Before 5.00 am" ~ "6 hours or less",    
    time_bed == "At midnight or later, but before 1.00 am" & time_wake == "At 5.00 am or later, but before 6.00 am" ~ "6 hours or less", 
    time_bed == "At midnight or later, but before 1.00 am" & time_wake == "At 6.00 am or later, but before 7.00 am" ~ "6 hours or less",
    time_bed == "At midnight or later, but before 1.00 am" & time_wake == "At 7.00 am or later, but before 8.00 am" ~ "Between 6 - 7 hours",
    time_bed == "At midnight or later, but before 1.00 am" & time_wake == "At 8.00 am or later" ~ "Between 7 - 8 hours",
    time_bed == "At 1.00 am or later, but before 2.00 am" & time_wake == "Before 5.00 am" ~ "6 hours or less",    
    time_bed == "At 1.00 am or later, but before 2.00 am" & time_wake == "At 5.00 am or later, but before 6.00 am" ~ "6 hours or less", 
    time_bed == "At 1.00 am or later, but before 2.00 am" & time_wake == "At 6.00 am or later, but before 7.00 am" ~ "6 hours or less",
    time_bed == "At 1.00 am or later, but before 2.00 am" & time_wake == "At 7.00 am or later, but before 8.00 am" ~ "6 hours or less",
    time_bed == "At 1.00 am or later, but before 2.00 am" & time_wake == "At 8.00 am or later" ~ "Between 6 - 7 hours",
    time_bed == "At 2.00 am or later" & time_wake == "Before 5.00 am" ~ "6 hours or less",    
    time_bed == "At 2.00 am or later" & time_wake == "At 5.00 am or later, but before 6.00 am" ~ "6 hours or less", 
    time_bed == "At 2.00 am or later" & time_wake == "At 6.00 am or later, but before 7.00 am" ~ "6 hours or less",
    time_bed == "At 2.00 am or later" & time_wake == "At 7.00 am or later, but before 8.00 am" ~ "6 hours or less",
    time_bed == "At 2.00 am or later" & time_wake == "At 8.00 am or later" ~ "6 hours or less"
  ))



### 5 - Define variables for analysis ----

variables <- data.frame(
  variable = c("general_health",
               "frequency_breakfast_weekday",
               "frequency_soft_drinks",
               "frequency_chips",
               "frequency_crisps",
               "frequency_sweets",
               "frequency_fruit",
               "frequency_vegetables",
               "frequency_being_hungry",
               "time_physical_activity",
               "frequency_physical_activity",
               "avg_hours_sleep"),
  cat_order = c("cat_order_1", 
                "cat_order_2", 
                "cat_order_3", 
                "cat_order_3", 
                "cat_order_3",
                "cat_order_3",
                "cat_order_4",
                "cat_order_4",
                "cat_order_5",
                "cat_order_6",
                "cat_order_7",
                "cat_order_8")
)



### 6 - Perform analysis on selected variables ----

# For national
national_physical_health <- perform_analysis_national(hwb_analysis, variables)

# For each local authority
local_authority_list <- list()

for (value in all_las) {
  cat("Processing local authority:", value, "\n")
  filtered_data <- hwb_analysis[hwb_analysis$pc_la == value, ]
  result <- perform_analysis_local_authority(filtered_data, variables)
  # Store the result with a dynamic name, e.g. Angus_physical_health
  list_name <- paste0(value, "_physical_health")
  local_authority_list[[list_name]] <- result
}



### 7 - Save combined_list as an excel file to Merged folder ----

# Save national
write_xlsx(
  national_physical_health,
  here("output", year, "National", "Output", paste0(year, "_physical_health.xlsx"))
)

# Save local authorities
# Function to save tibbles in respective folders
save_tibbles_as_sheets <- function(tibble_list, folder_name) {
  file_path <- here::here("output", year, folder_name, "Output", paste0(year, "_physical_health.xlsx"))
  write_xlsx(
    tibble_list,
    path = file_path,
    col_names = TRUE
  )
}

# Save each list of tibbles as a single Excel file with multiple sheets
map2(local_authority_list, all_las, save_tibbles_as_sheets)



### END OF SCRIPT ###
