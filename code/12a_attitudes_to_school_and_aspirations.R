#########################################################################
# Name of file - 12a_attitudes_to_school_and_aspirations.R
# Health and Wellbeing (HWB) Census 
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 4.2.2
#
# Description - Analyses date for attitudes to school and aspirations
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

cat_order_2 <- c("Not at all", 
                 "A little", 
                 "Some",
                 "A lot",
                 "Prefer not to say")



### 3 - Replace response values as per Measures for Inclusion in publication document ----

hwb_analysis[hwb_analysis == "Strongly agree"] <- "Strongly agree or Agree"
hwb_analysis[hwb_analysis == "Agree"] <- "Strongly agree or Agree"
hwb_analysis[hwb_analysis == "Strongly disagree"] <- "Strongly disagree or Disagree"
hwb_analysis[hwb_analysis == "Disagree"] <- "Strongly disagree or Disagree"



### 4 - Define variables for analysis ----

variables <- data.frame(
  variable = c("enjoy_learning_new_things", 
               "choice_of_learning", 
               "happy_at_school",
               "teachers_treat_me_fairly",
               "positive_about_future",
               "pressured_by_schoolwork",
               "have_adult_to_talk_to_school"),
  cat_order = c("cat_order_1", 
                "cat_order_1", 
                "cat_order_1", 
                "cat_order_1", 
                "cat_order_1", 
                "cat_order_2", 
                "cat_order_1")
)



### 5 - Perform analysis on selected variables ----

# For national
national_attitudes <- perform_analysis_national(hwb_analysis, variables)

# For each local authority
local_authority_list <- list()

for (value in all_las) {
  cat("Processing local authority:", value, "\n")
  filtered_data <- hwb_analysis[hwb_analysis$pc_la == value, ]
  result <- perform_analysis_local_authority(filtered_data, variables)
  # Store the result with a dynamic name, e.g. Angus_attitudes
  list_name <- paste0(value, "_attitudes")
  local_authority_list[[list_name]] <- result
}



### 6 - Save outputs as an excel file to Merged folder ----

# Save national
write_xlsx(
  national_attitudes,
  here("output", year, "National", paste0(year, "_attitudes_to_school_and_aspirations.xlsx"))
)

# Save local authorities
# Function to save tibbles in respective folders
save_tibbles_as_sheets <- function(tibble_list, folder_name) {
  file_path <- here::here("output", year, folder_name, paste0(year, "_attitudes_to_school_and_aspirations.xlsx"))
  write_xlsx(
    tibble_list,
    path = file_path,
    col_names = TRUE
  )
}

# Save each list of tibbles as a single Excel file with multiple sheets
map2(local_authority_list, all_las, save_tibbles_as_sheets)



### END OF SCRIPT ###

