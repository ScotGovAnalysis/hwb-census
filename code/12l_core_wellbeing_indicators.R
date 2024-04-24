#########################################################################
# Name of file - 12i_core_wellbeing_indicators.R
# Health and Wellbeing (HWB) Census 
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 4.2.2
#
# Description - Analyses data for core wellbeing indicators
#########################################################################


### 0 - Setup ----

source(here::here("code", "00_setup.R"))
source(here::here("functions", "read_raw_data.R"))
source(here::here("functions", "analysis_one_characteristic.R"))
source(here::here("functions", "analysis_stage_and_characteristic.R"))
source(here::here("functions", "perform_analysis_national.R"))
source(here::here("functions", "perform_analysis_local_authority.R"))



### 1 - Read in raw data ----

# Define the path to Excel file
file_path <- file.path(raw_data_folder, year, "Merged", "09_joined_stages.xlsx")

# Read in dataframe
hwb_analysis <- readxl::read_xlsx(file_path, sheet = 1)



### 2 - Set row order of response categories ----

cat_order_1 <- c("Participated in leisure activity",
                 "Did not participate")

cat_order_2 <- c("Both fruit and vegetables at least once day",
                 "Less frequent, including never")



### 3 - Create derived variable for 'Participated in a positive leisure activity this year' ----

# Extract columns that begin with "positive_activities"
positive_activities_cols <- grep("^positive_activities", names(hwb_analysis), value = TRUE)


# Create a new column 'Participated in a positive leisure activity this year'
hwb_analysis$'Participated in a positive leisure activity this year' <- apply(hwb_analysis[positive_activities_cols], 1, function(row) {
  if (all(row == "Question not asked of stage")) {
    return("Question not asked of stage")
  } else if (all(is.na(row))) {
    return(NA)
  } else if (all(row == "Data not collected")) {
    return("Data not collected")
  } else if ("Yes" %in% row[setdiff(names(row), "positive_activities_none")]) {
    return("Participated in leisure activity")
  } else {
    return("Did not participate")
  }
})



### 4 - Create derived variable for 'How often do you usually eat fruit and vegetables' ----

hwb_analysis$`How often do you usually eat fruit and vegetables` <- with(hwb_analysis, {
  ifelse(
    is.na(frequency_fruit) | is.na(frequency_vegetables),
    NA,
    ifelse(
      frequency_fruit %in% c("Question not asked of stage") | frequency_vegetables %in% c("Question not asked of stage"),
      "Question not asked of stage",
      ifelse(
        frequency_fruit %in% c("Data not collected") | frequency_vegetables %in% c("Data not collected"),
        "Data not collected",
        ifelse(
          frequency_fruit %in% c("At least once a day") & frequency_vegetables %in% c("At least once a day"),
          "Both fruit and vegetables at least once a day",
          "Less frequent, including never"
        )
      )
    )
  )
})



### 5 - Define variables for analysis ----

variables <- data.frame(
  variable = c("Participated in a positive leisure activity this year",
               "How often do you usually eat fruit and vegetables"),
  cat_order = c("cat_order_1",
                "cat_order_2")
)



### 6 - Perform analysis on selected variables ----

# For national
national_core_wellbeing_indicators <- perform_analysis_national(hwb_analysis, variables)

# For each local authority
local_authority_list <- list()

for (value in all_las) {
  cat("Processing local authority:", value, "\n")
  filtered_data <- hwb_analysis[hwb_analysis$pc_la == value, ]
  result <- perform_analysis_local_authority(filtered_data, variables)
  # Store the result with a dynamic name, e.g. Angus_core_wellbeing_indicators
  list_name <- paste0(value, "_core_wellbeing_indicators")
  local_authority_list[[list_name]] <- result
}



### 7 - Read in other core wellbeing indicators that were calculated in earlier scripts ----

all_topics <- c(
  "attitudes_to_school_and_aspirations", "experience_of_bullying", "mental_health_and_wellbeing",
  "mental_health_and_wellbeing_sdq", "mental_health_and_wellbeing_wemwbs", "neighbourhood_and_life_at_home",
  "physical_health", "relationships_and_sexual_health"
)

# For national
national_other_topics <- map(set_names(all_topics), function(topic) {
  file_path <- here("output", year, "National", paste0(year, "_", tolower(topic), ".xlsx"))
  # Get sheet names from the Excel file
  sheets <- excel_sheets(file_path)
  
  # Read each sheet into a named list
  sheet_data <- map(sheets, ~ read_xlsx(file_path, sheet = .x))
  
  # Set names for the sheet data based on sheet names
  names(sheet_data) <- sheets
  
  return(sheet_data)
})

# For LAs
# Define a function to read other topics analysis for each LA and store as a list of tibbles, only if that topic exists for that LA 
# (as e.g. East Renfrewshire does not have sexual health)
read_other_topics <- function(la) {
  map_if(
    set_names(all_topics),
    ~ file.exists(here(
      "output", 
      year,
      la,
      paste0(year, "_", tolower(.x), ".xlsx")
    )),
    ~ {
      file_path <- here(
        "output", 
        year,
        la,
        paste0(year, "_", tolower(.x), ".xlsx")
      )
      sheets <- excel_sheets(file_path)  # Get all sheet names
      tibbles <- map(sheets, ~ read_xlsx(file_path, sheet = .x))  # Read all sheets
      set_names(tibbles, sheets)  # Set original sheet names as names of tibbles
    }
  )
}

# Apply the function to all LAs in all_las and modify the names (for ease of joining later)
all_las_other_topics <- set_names(
  map(all_las, read_other_topics),
  paste0(all_las, "_core_wellbeing_indicators")
)



### 8 - Join on other core wellbeing indicators that were calculated in earlier scripts ----

# Function to filter and bind rows based on specified conditions for national
bind_rows_based_on_condition <- function(topic_name, survey_question) {
  for (i in seq_along(national_other_topics[[topic_name]])) {
    survey_question_filtered <- national_other_topics[[topic_name]][[i]] %>%
      filter(`Survey question` == survey_question)
    
    if (!is.null(survey_question_filtered)) {
      tibble_name <- names(national_other_topics[[topic_name]])[i]
      matching_tibble <- national_core_wellbeing_indicators[[tibble_name]]
      
      if (!is.null(matching_tibble)) {
        national_core_wellbeing_indicators[[tibble_name]] <- bind_rows(matching_tibble, survey_question_filtered)
      }
    }
  }
  return(national_core_wellbeing_indicators)
}

# Function to filter and bind rows based on specified conditions for local authorities
bind_rows_based_on_condition_la <- function(topic_name, survey_question) {
  for (i in seq_along(all_las_other_topics)) {
    for (j in seq_along(all_las_other_topics[[i]][[topic_name]])) {
      survey_question_filtered <- all_las_other_topics[[i]][[topic_name]][[j]] %>%
        filter(`Survey question` == survey_question)
      
      if (!is.null(survey_question_filtered)) {
        tibble_name <- names(all_las_other_topics[[i]][[topic_name]])[j]
        matching_tibble <- local_authority_list[[i]][[tibble_name]]
        
        if (!is.null(matching_tibble)) {
          local_authority_list[[i]][[tibble_name]] <- bind_rows(matching_tibble, survey_question_filtered)
        }
      }
    }
  }
  return(local_authority_list)
}

indicators <- list(
  c("physical_health", "time_physical_activity"),
  c("mental_health_and_wellbeing_sdq", "Total difficulties score % - four band scale"),
  c("neighbourhood_and_life_at_home", "adult_to_trust"),
  c("neighbourhood_and_life_at_home", "friends_treat_me_well"),
  c("mental_health_and_wellbeing", "adults_taking_into_account"),
  c("experience_of_bullying", "bullied_last_year"),
  c("neighbourhood_and_life_at_home", "my_area_good_place"),
  c("neighbourhood_and_life_at_home", "internet_access"),
  c("neighbourhood_and_life_at_home", "my_area_feeling_safe"),
  c("mental_health_and_wellbeing_wemwbs", "Average WEMWBS score"),
  c("mental_health_and_wellbeing_wemwbs", "Total")
)

for (ind in indicators) {
  national_core_wellbeing_indicators <- bind_rows_based_on_condition(ind[1], ind[2])
}

for (ind in indicators) {
  local_authority_list <- bind_rows_based_on_condition_la(ind[1], ind[2])
}



# 9 - Save combined_list as an excel file to Merged folder ----

# Save national
write_xlsx(
  national_core_wellbeing_indicators,
  here("output", year, "National", paste0(year, "_core_wellbeing_indicators.xlsx"))
)

# Save local authorities
# Function to save tibbles in respective folders
save_tibbles_as_sheets <- function(tibble_list, folder_name) {
  file_path <- here::here("output", year, folder_name, paste0(year, "_core_wellbeing_indicators.xlsx"))
  write_xlsx(
    tibble_list,
    path = file_path,
    col_names = TRUE
  )
}

# Save each list of tibbles as a single Excel file with multiple sheets
map2(local_authority_list, all_las, save_tibbles_as_sheets)


### END OF SCRIPT ###
