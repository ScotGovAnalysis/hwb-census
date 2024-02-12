#########################################################################
# Name of file - 12f_neighbourhood_and_life_at_home.R
# Health and Wellbeing (HWB) Census 
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 4.2.2
#
# Description - Analyses data for neighbourhood and life at home
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

cat_order_2 <- c("Every day", 
                 "Most days", 
                 "About once a week",
                 "Less than once a week",
                 "Never")

cat_order_3 <- c("Always", 
                 "Most of the time", 
                 "Sometimes",
                 "Rarely or Never",
                 "Prefer not to say")

cat_order_4 <- c("Yes, it's good", 
                 "It's OK", 
                 "No, it's not good",
                 "Prefer not to say")

cat_order_5 <- c("Always", 
                 "Often", 
                 "Sometimes",
                 "Never",
                 "Prefer not to say")

cat_order_6 <- c("Every day", 
                 "Most days", 
                 "About once a week",
                 "Less than once a week",
                 "Never",
                 "Prefer not to say")

cat_order_7 <- c("Easy", 
                 "Difficult", 
                 "Does not apply to me")

cat_order_8 <- c("Yes, I always do", 
                 "Yes, I sometimes do", 
                 "No, I don't",
                 "Prefer not to say")

cat_order_9 <- c("Three or more", 
                 "Two", 
                 "One",
                 "None",
                 "Prefer not to say")

cat_order_10 <- c("Yes", 
                  "No", 
                  "Prefer not to say")

cat_order_11 <- c("Yes", 
                  "No")



### 3 - Replace response values as per Measures for Inclusion in publication document ----

hwb_analysis[hwb_analysis == "Strongly agree"] <- "Strongly agree or Agree"
hwb_analysis[hwb_analysis == "Agree"] <- "Strongly agree or Agree"
hwb_analysis[hwb_analysis == "Strongly disagree"] <- "Strongly disagree or Disagree"
hwb_analysis[hwb_analysis == "Disagree"] <- "Strongly disagree or Disagree"



### 4 - Define variables for analysis ----

variables <- data.frame(
  variable = c("my_area_feeling_safe", 
               "my_area_good_place", 
               "frequency_enjoying_family",
               "frequency_meals_together",
               "easy_to_talk_to_another_adult",
               "easy_to_talk_to_mum",
               "easy_to_talk_to_dad",
               "adult_to_trust",
               "number_close_friends",
               "friends_treat_me_well",
               "money_to_do_same_things_friends",
               "positive_activities_mentoring",
               "positive_activities_volunteering",
               "positive_activities_charity_events",
               "positive_activities_performing_arts_group",
               "positive_activities_religious_activity",
               "positive_activities_youth_organisation",
               "positive_activities_doe",
               "positive_activities_sports_clubs",
               "positive_activities_none_of_above",
               "internet_access"),
  cat_order = c("cat_order_3", 
                "cat_order_4", 
                "cat_order_5", 
                "cat_order_6", 
                "cat_order_7",
                "cat_order_7", 
                "cat_order_7", 
                "cat_order_8", 
                "cat_order_9", 
                "cat_order_1",
                "cat_order_11", 
                "cat_order_11", 
                "cat_order_11", 
                "cat_order_11", 
                "cat_order_11",
                "cat_order_11", 
                "cat_order_11", 
                "cat_order_11", 
                "cat_order_11", 
                "cat_order_11",
                "cat_order_10"
                )
)



### 5 - Perform analysis on selected variables ----

# For national
national_neighbourhood <- perform_analysis_national(hwb_analysis, one_characteristics, stage_and_characteristics, variables)

# For each local authority
local_authority_list <- list()

for (value in all_las) {
  cat("Processing local authority:", value, "\n")
  filtered_data <- hwb_analysis[hwb_analysis$pc_la == value, ]
  result <- perform_analysis_local_authority(filtered_data, one_characteristics, stage_and_characteristics, variables)
  # Store the result with a dynamic name, e.g. Angus_neighbourhood
  list_name <- paste0(value, "_neighbourhood")
  local_authority_list[[list_name]] <- result
}



### 6 - Filter out rows we don't want and format derived questions ----

# Questions which were tick boxes in SmartSurvey need to be reformatted from multiple questions to one
# Define a function to filter rows based on specified conditions
filter_rows <- function(tibble) {
  tibble %>%
    filter(!(`Survey question` == 'positive_activities_mentoring' & `Response` == 'No')) %>%
    filter(!(`Survey question` == 'positive_activities_mentoring' & `Response` == 'Total')) %>%
    filter(!(`Survey question` == 'positive_activities_volunteering' & `Response` == 'No')) %>%
    filter(!(`Survey question` == 'positive_activities_volunteering' & `Response` == 'Total')) %>%
    filter(!(`Survey question` == 'positive_activities_charity_events' & `Response` == 'No')) %>%
    filter(!(`Survey question` == 'positive_activities_charity_events' & `Response` == 'Total')) %>%
    filter(!(`Survey question` == 'positive_activities_performing_arts_group' & `Response` == 'No')) %>%
    filter(!(`Survey question` == 'positive_activities_performing_arts_group' & `Response` == 'Total')) %>%
    filter(!(`Survey question` == 'positive_activities_religious_activity' & `Response` == 'No')) %>%
    filter(!(`Survey question` == 'positive_activities_religious_activity' & `Response` == 'Total')) %>%
    filter(!(`Survey question` == 'positive_activities_youth_organisation' & `Response` == 'No')) %>%
    filter(!(`Survey question` == 'positive_activities_youth_organisation' & `Response` == 'Total')) %>%
    filter(!(`Survey question` == 'positive_activities_doe' & `Response` == 'No')) %>%
    filter(!(`Survey question` == 'positive_activities_doe' & `Response` == 'Total')) %>%
    filter(!(`Survey question` == 'positive_activities_sports_clubs' & `Response` == 'No')) %>%
    filter(!(`Survey question` == 'positive_activities_sports_clubs' & `Response` == 'Total')) %>%
    filter(!(`Survey question` == 'positive_activities_none_of_above' & `Response` == 'No'))
}

# Apply the function to each tibble in the list national_neighbourhood
national_neighbourhood <- map(national_neighbourhood, ~ filter_rows(.))

# Apply the function to each tibble in the list of lists local_authority_list
local_authority_list <- lapply(local_authority_list, function(sublist) {
  map(sublist, filter_rows)
})


# Function to replace values in 'Response' column
replace_values <- function(tibble) {
  tibble$Response[tibble$'Survey question' == 'positive_activities_mentoring' & tibble$Response == 'Yes'] <- 'Taken part in the buddying/mentoring programme at school'
  tibble$Response[tibble$'Survey question' == 'positive_activities_volunteering' & tibble$Response == 'Yes'] <- 'Done voluntary work'
  tibble$Response[tibble$'Survey question' == 'positive_activities_charity_events' & tibble$Response == 'Yes'] <- 'Taken part in a charity event'
  tibble$Response[tibble$'Survey question' == 'positive_activities_performing_arts_group' & tibble$Response == 'Yes'] <- 'Taken part in a drama / acting / singing / dancing group'
  tibble$Response[tibble$'Survey question' == 'positive_activities_religious_activity' & tibble$Response == 'Yes'] <- 'Taken part in a religious activity (e.g. Church service, Scripture Union, Quran classes)'
  tibble$Response[tibble$'Survey question' == 'positive_activities_youth_organisation' & tibble$Response == 'Yes'] <- 'Attended a youth organisation (e.g. Boys or Girls Brigade, Scouts, Girl Guides, etc.)'
  tibble$Response[tibble$'Survey question' == 'positive_activities_doe' & tibble$Response == 'Yes'] <- 'Duke of Edinburgh'
  tibble$Response[tibble$'Survey question' == 'positive_activities_sports_clubs' & tibble$Response == 'Yes'] <- 'Sports clubs'
  tibble$Response[tibble$'Survey question' == 'positive_activities_none_of_above' & tibble$Response == 'Yes'] <- 'None of the above'
  return(tibble)
}

# Apply the function to each tibble in the list national_neighbourhood
national_neighbourhood <- map(national_neighbourhood, replace_values)

# Apply the function to each tibble in the list of lists local_authority_list
local_authority_list <- map(local_authority_list, function(inner_list) {
  map(inner_list, replace_values)
})


# Function to replace values in 'Survey question' column
replace_survey_question <- function(tibble) {
  tibble <- tibble %>%
    mutate(`Survey question` = case_when(
      `Survey question` %in% c("positive_activities_mentoring", "positive_activities_volunteering", "positive_activities_charity_events",
                               "positive_activities_performing_arts_group", "positive_activities_religious_activity", "positive_activities_youth_organisation",
                               "positive_activities_doe", "positive_activities_sports_clubs", "positive_activities_none_of_above") ~ "Which, if any, of these things have you done in the last year?",
      TRUE ~ `Survey question`
    ))
  return(tibble)
}

# Replace values in 'Survey question' column for each tibble in the list national_neighbourhood
national_neighbourhood <- map(national_neighbourhood, replace_survey_question)

# Replace values in 'Survey question' column for each tibble in the list of lists local_authority_list
local_authority_list <- map(local_authority_list, function(inner_list) {
  map(inner_list, replace_survey_question)
})



### 7 - Save outputs as an excel file to Merged folder ----

# Save national
write_xlsx(
  national_neighbourhood,
  here("output", year, "National", "Output", paste0(year, "_neighbourhood_and_life_at_home.xlsx"))
)

# Save local authorities
# Function to save tibbles in respective folders
save_tibbles_as_sheets <- function(tibble_list, folder_name) {
  file_path <- here::here("output", year, folder_name, "Output", paste0(year, "_neighbourhood_and_life_at_home.xlsx"))
  write_xlsx(
    tibble_list,
    path = file_path,
    col_names = TRUE
  )
}

# Save each list of tibbles as a single Excel file with multiple sheets
map2(local_authority_list, all_las, save_tibbles_as_sheets)


### END OF SCRIPT ###

