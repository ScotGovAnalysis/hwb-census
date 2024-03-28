#########################################################################
# Name of file - 12b_experience_of_bullying.R
# Health and Wellbeing (HWB) Census 
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 4.2.2
#
# Description - Analyses data for experience of bullying
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

cat_order_2 <- c("Yes", 
                 "No")

cat_order_3 <- c("Most days",
                "About once a week",
                "About once a month",
                "Every few months",
                "Never",
                "Prefer not to say")



### 3 - Replace response values as per Measures for Inclusion in publication document ----

hwb_analysis[hwb_analysis == "Once a week or more"] <- "Most days or About once a week"
hwb_analysis[hwb_analysis == "Once a month or every few months"] <- "About once a month or Every few months"
hwb_analysis[hwb_analysis == "Never"] <- "Never"



### 4 - Define variables for analysis ----

variables <- data.frame(
  variable = c("bullied_last_year", 
               "bullying_location_school", 
               "bullying_location_elsewhere",
               "bullying_location_online",
               "frequency_bullied_online"),
  cat_order = c("cat_order_1", 
                "cat_order_2", 
                "cat_order_2", 
                "cat_order_2", 
                "cat_order_3")
)



### 5 - Perform analysis on selected variables ----

# For national
national_bullying <- perform_analysis_national(hwb_analysis, variables)

# For each local authority
local_authority_list <- list()

for (value in all_las) {
  cat("Processing local authority:", value, "\n")
  filtered_data <- hwb_analysis[hwb_analysis$pc_la == value, ]
  result <- perform_analysis_local_authority(filtered_data, variables)
  # Store the result with a dynamic name, e.g. Angus_bullying
  list_name <- paste0(value, "_bullying")
  local_authority_list[[list_name]] <- result
}



### 6 - Filter out rows we don't want and format derived questions ----

# Questions which were tick boxes in SmartSurvey need to be reformatted from multiple questions to one
# Define a function to filter rows based on specified conditions
filter_rows <- function(tibble) {
  tibble %>%
    filter(!(`Survey question` == 'bullying_location_school' & `Response` == 'No')) %>%
    filter(!(`Survey question` == 'bullying_location_school' & `Response` == 'Total')) %>%
    filter(!(`Survey question` == 'bullying_location_elsewhere' & `Response` == 'No')) %>%
    filter(!(`Survey question` == 'bullying_location_elsewhere' & `Response` == 'Total')) %>%
    filter(!(`Survey question` == 'bullying_location_online' & `Response` == 'No'))
}

# Apply the function to each tibble in the list national_bullying
national_bullying <- map(national_bullying, ~ filter_rows(.))

# Apply the function to each tibble in the list of lists local_authority_list
local_authority_list <- lapply(local_authority_list, function(sublist) {
  map(sublist, filter_rows)
})


# Function to replace values in 'Response' column
replace_values <- function(tibble) {
  tibble$Response[tibble$'Survey question' == 'bullying_location_school' & tibble$Response == 'Yes'] <- 'At school'
  tibble$Response[tibble$'Survey question' == 'bullying_location_elsewhere' & tibble$Response == 'Yes'] <- 'Somewhere else (including on the way to or from school)'
  tibble$Response[tibble$'Survey question' == 'bullying_location_online' & tibble$Response == 'Yes'] <- 'Online / social media / gaming platform'
  return(tibble)
}

# Apply the function to each tibble in the list national_bullying
national_bullying <- map(national_bullying, replace_values)

# Apply the function to each tibble in the list of lists local_authority_list
local_authority_list <- map(local_authority_list, function(inner_list) {
  map(inner_list, replace_values)
})


# Function to replace values in 'Survey question' column
replace_survey_question <- function(tibble) {
  tibble <- tibble %>%
    mutate(`Survey question` = case_when(
      `Survey question` %in% c("bullying_location_school", "bullying_location_elsewhere", "bullying_location_online") ~ "Where have you been bullied?",
      TRUE ~ `Survey question`
    ))
  return(tibble)
}

# Replace values in 'Survey question' column for each tibble in the list national_bullying
national_bullying <- map(national_bullying, replace_survey_question)

# Replace values in 'Survey question' column for each tibble in the list of lists local_authority_list
local_authority_list <- map(local_authority_list, function(inner_list) {
  map(inner_list, replace_survey_question)
})



### 7 - Save outputs as an excel file to Merged folder ----

# Save national
write_xlsx(
  national_bullying,
  here("output", year, "National", paste0(year, "_experience_of_bullying.xlsx"))
)

# Save local authorities
# Function to save tibbles in respective folders
save_tibbles_as_sheets <- function(tibble_list, folder_name) {
  file_path <- here::here("output", year, folder_name, paste0(year, "_experience_of_bullying.xlsx"))
  write_xlsx(
    tibble_list,
    path = file_path,
    col_names = TRUE
  )
}

# Save each list of tibbles as a single Excel file with multiple sheets
map2(local_authority_list, all_las, save_tibbles_as_sheets)


### END OF SCRIPT ###

