#########################################################################
# Name of file - 13a_formatting_and_suppression_hwb.R
# Health and Wellbeing (HWB) Census 
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 4.2.2
#
# Description - Formats and suppresses tables for publication for HWB data
#########################################################################



### 0 - Setup ----

source(here::here("code", "00_setup.R"))
source(here::here("functions", "read_raw_data.R"))
source(here::here("functions", "perform_data_suppression.R"))
source(here::here("functions", "perform_formatting.R"))
source(here::here("functions", "reorder_columns.R"))



### 1 - Read in raw data ----

read_excel_files <- function(directory) {
  excel_files <- list.files(path = directory, pattern = "\\.xlsx$", full.names = TRUE)
  raw_data <- list()
  
  for (file in excel_files) {
    if (!grepl("core_wellbeing_indicators", file) & !grepl("substance_use", file) & !grepl("carers_analysis", file)) {  # Check if the file name contains "core_wellbeing_indicators" or "substance_use" or "carers_analysis"
      cat("Reading file:", file, "\n")
      
      file_name <- tools::file_path_sans_ext(basename(file))
      sheets <- readxl::excel_sheets(file)
      sheet_data <- list()
      
      for (sheet in sheets) {
        sheet_data[[sheet]] <- readxl::read_excel(file, sheet = sheet)
      }
      
      raw_data[[file_name]] <- sheet_data
    } else {
      cat("Skipping file containing 'core_wellbeing_indicators' or 'substance_use' or 'carers_analysis':", file, "\n")
    }
  }
  
  return(raw_data)
}

# Initialize an empty list to store all raw data
all_raw_data <- list()

# Loop through each directory
for (las in all_las) {
  # Set the file path for the current directory
  file_path <- here("output", year, las)
  
  # Inform user about the directory being processed
  cat("Processing directory:", file_path, "\n")
  
  # Read data for the current directory
  raw_data <- read_excel_files(file_path)
  
  # Append the results to the list
  all_raw_data[[las]] <- raw_data
}

# Read data from the "National" folder and add to all_raw_data list
national_file_path <- here("output", year, "National")
cat("Processing directory:", national_file_path, "\n")
national_raw_data <- read_excel_files(national_file_path)
all_raw_data[["National"]] <- national_raw_data



### 2 - Read in survey questions for publication metadata ----

# This is used to replace values in the 'Survey question' column when formatting
survey_question_metadata <-
  read_xlsx(here("metadata",
                 year,
                 "hwb_metadata_survey_questions_for_publication.xlsx"))



### 3 - Join dataframes for each local authority together and dataframes for national together ----

# Function to add 'Survey topic' column as the first column in each tibble
add_survey_topic_column <- function(data, topic_name) {
  lapply(data, function(tbl) {
    tbl <- cbind(`Survey topic` = rep(topic_name, each = nrow(tbl)), tbl)
    tbl
  })
}

# Loop through each top-level list in all_raw_data
for (region_name in names(all_raw_data)) {
  region <- all_raw_data[[region_name]]
  
  # Loop through sublists within each top-level list
  for (sublist_name in names(region)) {
    sublist <- region[[sublist_name]]
    
    # Check if sublist is a list
    if (is.list(sublist)) {
      # Add 'Survey topic' column to each tibble in the sublist
      all_raw_data[[region_name]][[sublist_name]] <- add_survey_topic_column(sublist, sublist_name)
    }
  }
}

# Loop through each top-level list in national_raw_data
for (topic_name in names(national_raw_data)) {
  topic <- national_raw_data[[topic_name]]
  
  # Check if topic is a list
  if (is.list(topic)) {
    # Add 'Survey topic' column to each tibble in the sublist
    national_raw_data[[topic_name]] <- add_survey_topic_column(topic, topic_name)
  }
}

# Function to row bind tibbles with the same name
bind_same_name <- function(lst) {
  # Get unique names of tibbles
  tibble_names <- names(lst[[1]])

  # Row bind tibbles with the same name, storing the result in a named list
  tibble_list <- map(tibble_names, ~ map_dfr(lst, .x))

  # Name the list elements after the original tibble names
  names(tibble_list) <- tibble_names

  # Return the row binded tibbles in a named list
  return(tibble_list)
}

# Apply bind_same_name function to each sub-list in all_raw_data
combined_data_las <- map(all_raw_data, bind_same_name)

# Apply bind_same_name function to national_raw_data
combined_data_national <- bind_same_name(national_raw_data)


# Function to clean the 'Survey topic' values from "2022_attitudes_to_school_and_aspirations" to "Attitudes to School and Aspirations"
clean_survey_topic <- function(topic_name) {
  # Remove numeric characters and first "_", replace remaining "_" with " ", and capitalize the first letter
  cleaned_topic <- gsub("^\\d+_", "", topic_name)  # Remove leading numeric characters followed by "_"
  cleaned_topic <- gsub("_", " ", cleaned_topic)  # Replace remaining "_" with " "
  cleaned_topic <- tools::toTitleCase(cleaned_topic)  # Capitalize the first letter of each word

  # Capitalize specific strings
  cleaned_topic <- gsub("\\bsdq\\b", "SDQ", cleaned_topic, ignore.case = TRUE)
  cleaned_topic <- gsub("\\bwemwbs\\b", "WEMWBS", cleaned_topic, ignore.case = TRUE)

  cleaned_topic
}

# Loop through each top-level list in combined_data_las
for (region_name in names(combined_data_las)) {
  region <- combined_data_las[[region_name]]

  # Loop through sublists within each top-level list
  for (sublist_name in names(region)) {
    sublist <- region[[sublist_name]]

    # Check if sublist is a list
    if (is.list(sublist)) {
      # Clean up the 'Survey topic' column values
      combined_data_las[[region_name]][[sublist_name]][['Survey topic']] <- clean_survey_topic(combined_data_las[[region_name]][[sublist_name]][['Survey topic']])
    }
  }
}

# Loop through each top-level list in combined_data_national
for (topic_name in names(combined_data_national)) {
  topic <- combined_data_national[[topic_name]]
    
    # Check if sublist is a list
    if (is.list(sublist)) {
      # Clean up the 'Survey topic' column values
      combined_data_national[[topic_name]][['Survey topic']] <- clean_survey_topic(combined_data_national[[topic_name]][['Survey topic']])
    }
}



### 4 - Perform data suppression ----

# For local authorities
las_suppressed <- lapply(combined_data_las, perform_data_suppression)

# For national
national_suppressed <- perform_data_suppression(combined_data_national)



### 5 - Format data as dataframes ----

# For local authorities
formatted_data_las <- lapply(las_suppressed, perform_formatting)

# For national
formatted_data_national <- perform_formatting(national_suppressed)



### 6 - Re-order columns ----

# Apply the reorder_columns function to each dataframe in the list formatted_data_national
formatted_data_national <- lapply(formatted_data_national, reorder_columns)

# Apply the reorder_columns function to the list of lists of dataframes formatted_data_las
formatted_data_las <- lapply(formatted_data_las, function(inner_list) {
  # Apply the reorder_columns function to each dataframe in the inner list
  reordered_inner_list <- lapply(inner_list, reorder_columns)
  return(reordered_inner_list)
})



### 7 - Produce a11ytables template and convert to workbook ----

cover_list <- list(
   "Summary Statistics for Health and Wellbeing Census 2021-22" = 
    c("This spreadsheet contains summary statistics for the data collected as part of the Health and Wellbeing Census:", 
      "[Health and Wellbeing Census - gov.scot (www.gov.scot)](https://www.gov.scot/publications/health-and-wellbeing-census-2/)",
      "The statistics presented are experimental statistics.",
      "The information presented in this spreadsheet relates to pupils of publicly-funded mainstream primary and secondary schools in Scotland.",
      "This spreadsheet contains a range of survey question items. It gives breakdowns by a variety of pupil characteristics including sex, ethnic group, urban rural classificiation, Scottish Index of Multiple Deprivation (SIMD), long-term health condition and caring responsibilities.",
      "For commentary and background notes on these statistics, please refer to the main publication website:",
      "[Health and Wellbeing Census Scotland 2021-2022](https://www.gov.scot/publications/health-and-wellbeing-census-scotland-2021-22/)"
      ),
  "Participating Local Authorities" = "The following local authority councils participated and provided data for this publication: Angus, Clackmannanshire, Dumfries & Galloway, Dundee, East Renfrewshire, Edinburgh City, Falkirk, Glasgow, Moray, North Ayrshire, Perth & Kinross, Renfrewshire, Scottish Borders, Shetland, South Ayrshire and Stirling.",
  "Publication dates" = "The data tables in this spreadsheet were originally published at 9:30am on 23rd May 2023.",
  "Note on the impact of COVID-19" = "The impact of Covid-19 on schools and education has been significant, and schools have responded by putting in place different ways of working over the changing landscape since early 2020. This will affect the data and needs to be taken into account in understanding and using the results. The questions are designed to capture children and young people’s responses at the time of data collection (October 2021 – June 2022).",
  "Notes, symbols used and cells with no data" = 
    c("Some cells in the tables refer to notes which can be found in the notes worksheet. Note markers are presented in square brackets, for example: [note 1].",
      "Some tables in this spreadsheet have cells with the following symbol: [c] where the value is suppressed to protect against the risk of disclosure of personal information. This may include suppression of cases where the value is zero (0.0). Where a value is \"-\", data was not collected for this question.",
      "These symbols have been chosen to align with harmonisation standards set by the Government Statistical Service."),
  "Corrections" = "No corrections have been made to date.",
  "Contact details" = "[schoolstats@gov.scot](mailto:schoolstats@gov.scot)"
)

contents_la_df <- data.frame(
  "Worksheet number" = c("Notes", paste("Table", 1:7, sep = "_")),
  "Worksheet title" = c(
    "Notes",
    "Percentage by question and stage",
    "Percentage by question and sex",
    "Percentage by question and ethnic group",
    "Percentage by question and urban rural classification",
    "Percentage by question and SIMD",
    "Percentage by question and long-term health condition",
    "Percentage by question and caring responsibilities"
  ),
  "Date this data was first published" = rep("23/05/2023", times = 8),
  "Date updated" = rep("", 8),
  check.names = FALSE
)

contents_national_df <- data.frame(
  "Worksheet number" = c("Notes", paste("Table", 1:16, sep = "_")),
  "Worksheet title" = c(
    "Notes",
    "Percentage by stage",
    "Percentage by local authority [note 3]",
    "Percentage by sex [note 1]",
    "Percentage by stage and sex [note 1]",
    "Percentage by SIMD [note 1] [note 2]",
    "Percentage by stage and SIMD [note 1] [note 2]",
    "Percentage by urban rural classification [note 1] [note 4]",
    "Percentage by stage and urban rural classification [note 1] [note 4]",
    "Percentage by ethnic group [note 1]",
    "Percentage by stage and ethnic group [note 1]",
    "Percentage by Additional Support Needs [note 1]",
    "Percentage by stage and Additional Support Needs [note 1]",
    "Percentage by caring responsibilities [note 5]",
    "Percentage by stage and caring responsibilities [note 5]",
    "Percentage by long term health condition [note 6]",
    "Percentage by stage and long term health condition [note 6]"
  ),
  "Date this data was first published" = rep("23/05/2023", times = 17),
  "Date updated" = rep("", 17),
  check.names = FALSE
)

notes_la_df <- data.frame(
  "Note number" = paste0("[note ", 1:13, "]"),
  "Note text" = c("The characteristics are attached by linking the HWB Census data with the Pupil Census 2021. Only records with a unique valid Scottish Candidate Number (SCN) can be linked. Where SCNs were not collected or where characteristics data was not available in the Pupil Census, records were included as \"Not known\".",
                  "The urban/rural classifications in Pupil Census are based on the 2016 urban rural classification.", 
                  "Based on Scottish Index of Multiple Deprivation (SIMD) SIMD 2020. For more information see the Scottish Government's website.",
                  "This is a self identified long term health condition question. This question is asked of P5 - S6. The question asks \"Do you have a physical or mental health condition or illness lasting or expected to last 12 months or more?\"",
                  "This is a self identified caring responsibility. Caring responsibilities are asked in the stage questionnaires for P7 - S6. The question asks \"Do you care for, or look after, someone? For example, because they have a disability, an illness, a drug or alcohol problem, a mental health problem, or problems related to old age.\"",
                  "The response options for P5 - P6 are: Every day / At least once a week but not every day / At least once a month but not every week / Less than once a month / Never / Prefer not to say. \n The response options for P7 - S6 are: Every day / 4 to 6 times a week / 2 to 3 times a week / Once a week / At least once a month but not every week / Less than once a month / Never / Prefer not to say. These have been aggregated Every day / at least once a week / less than once a week / less than once a month. At least once a week is \"At least once a week but not every day\" and \"2 to 3 times a week\" and \"4 to 6 times a week\". Less than once a week is \"At least once a month but not every week\" and \"less than once a month\".",
                  "Mean hours sleep is calculated from \"When do you usually go to bed if you have to go to school the next morning?\" and \"When do you usually wake up on school mornings?\" questions.",
                  "This question had different response options across the stages: \n P5 – P7: Agree, Neither agree nor disagree, Disagree, Don't know \n S1 – S6: Agree, Disagree, Don't know",
                  "This question had different response options across the stages: \n P5 – S1: Strongly agree, Agree, Neither agree nor disagree, Disagree, Strongly disagree \n S2 – S6: Strongly agree, Agree, Neither agree nor disagree, Disagree, Strongly disagree, Prefer not to say",
                  "This question was asked of P5 - P7, with the response options None at all / Some time (up to 2 hours a day) / Quite a bit of time (about 3 hours a day or more)",
                  "This question was asked of S1 - S6, with the response options None at all / About half an hour	 / About 1 hour a day / About 2 hours a day / About 3 hours a day / About 4 hours a day / About 5 hours a day / About 6 hours a day / About 7 hours or more a day.",
                  "This is calculated from the question \"During the past year, have you... regularly found that you can’t think of anything else but the moment that you will be able to use social media again? / ...regularly felt dissatisfied because you wanted to spend more time on social media? / ...often felt bad when you could not use social media? / ...tried to spend less time on social media but failed? / ...regularly neglected other activities (e.g. hobbies, sport) because you wanted to use social media? / ...regularly had arguments with others because of your social media use? / ...regularly lied to your parents or friends about the amount of time you spend on social media? / ...often used social media to escape from negative feelings? / ...had serious conflict with your parents, brother(s) or sister(s) because of your social media use? (No / Yes)\". \n Each item responded to with 'yes' scores a value of 1, 'no' responses score 0. \n Problematic use is classified as responding ‘yes’ to at least 6 of the 9 items. \n Mean score can be calculated across the cohort, for LA and national level.",
                  "The \"Not known\" category in this breakdown consists of \"Prefer not to say\" responses and those where no response was recorded for the long-term health condition/caring responsibility question. The grouping of these records was applied in order to avoid high levels of suppression of data."
                  ),
  "Relevant hyperlinks (where appropriate)" = c("More information on the Pupil Census",
                                                "More information on the urban rural classification 2016",
                                                "More information on the Scottish Index of Multiple Deprivation (SIMD)",
                                                rep("", 10)
                                                ),
  "url" = c("https://www.gov.scot/publications/pupil-census-supplementary-statistics/",
            "https://www.gov.scot/publications/scottish-government-urban-rural-classification-2016/",
            "https://www.gov.scot/collections/scottish-index-of-multiple-deprivation-2020/",
            rep("", 10)
  ),
  check.names = FALSE
)

notes_national_df <- data.frame(
  "Note number" = paste0("[note ", 1:14, "]"),
  "Note text" = c("The characteristics are attached by linking the HWB Census data with the Pupil Census 2021. Only records with a unique valid Scottish Candidate Number (SCN) can be linked. Where SCNs were not collected or where characteristics data was not available in the Pupil Census, records were included as \"Not known\".",
                  "The urban/rural classifications in Pupil Census are based on the 2016 urban rural classification.", 
                  "Based on Scottish Index of Multiple Deprivation (SIMD) SIMD 2020. For more information see the Scottish Government's website.",
                  "This is a self identified long term health condition question. This question is asked of P5 - S6. The question asks \"Do you have a physical or mental health condition or illness lasting or expected to last 12 months or more?\"",
                  "This is a self identified caring responsibility. Caring responsibilities are asked in the stage questionnaires for P7 - S6. The question asks \"Do you care for, or look after, someone? For example, because they have a disability, an illness, a drug or alcohol problem, a mental health problem, or problems related to old age.\"",
                  "The response options for P5 - P6 are: Every day / At least once a week but not every day / At least once a month but not every week / Less than once a month / Never / Prefer not to say. \n The response options for P7 - S6 are: Every day / 4 to 6 times a week / 2 to 3 times a week / Once a week / At least once a month but not every week / Less than once a month / Never / Prefer not to say. These have been aggregated Every day / at least once a week / less than once a week / less than once a month. At least once a week is \"At least once a week but not every day\" and \"2 to 3 times a week\" and \"4 to 6 times a week\". Less than once a week is \"At least once a month but not every week\" and \"less than once a month\".",
                  "Mean hours sleep is calculated from \"When do you usually go to bed if you have to go to school the next morning?\" and \"When do you usually wake up on school mornings?\" questions.",
                  "This question had different response options across the stages: \n P5 – P7: Agree, Neither agree nor disagree, Disagree, Don't know \n S1 – S6: Agree, Disagree, Don't know",
                  "This question had different response options across the stages: \n P5 – S1: Strongly agree, Agree, Neither agree nor disagree, Disagree, Strongly disagree \n S2 – S6: Strongly agree, Agree, Neither agree nor disagree, Disagree, Strongly disagree, Prefer not to say",
                  "This question was asked of P5 - P7, with the response options None at all / Some time (up to 2 hours a day) / Quite a bit of time (about 3 hours a day or more)",
                  "This question was asked of S1 - S6, with the response options None at all / About half an hour	 / About 1 hour a day / About 2 hours a day / About 3 hours a day / About 4 hours a day / About 5 hours a day / About 6 hours a day / About 7 hours or more a day.",
                  "This is calculated from the question \"During the past year, have you... regularly found that you can’t think of anything else but the moment that you will be able to use social media again? / ...regularly felt dissatisfied because you wanted to spend more time on social media? / ...often felt bad when you could not use social media? / ...tried to spend less time on social media but failed? / ...regularly neglected other activities (e.g. hobbies, sport) because you wanted to use social media? / ...regularly had arguments with others because of your social media use? / ...regularly lied to your parents or friends about the amount of time you spend on social media? / ...often used social media to escape from negative feelings? / ...had serious conflict with your parents, brother(s) or sister(s) because of your social media use? (No / Yes)\". \n Each item responded to with 'yes' scores a value of 1, 'no' responses score 0. \n Problematic use is classified as responding ‘yes’ to at least 6 of the 9 items. \n Mean score can be calculated across the cohort, for LA and national level.",
                  "The \"Not known\" category in this breakdown consists of \"Prefer not to say\" responses and those where no response was recorded for the long-term health condition/caring responsibility question. The grouping of these records was applied in order to avoid high levels of suppression of data.",
                  "Analysis by local authority includes both responses linked to the pupil census 2021 and those responses where SCNs were not collected."
  ),
  "Relevant hyperlinks (where appropriate)" = c("More information on the Pupil Census",
                                                "More information on the urban rural classification 2016",
                                                "More information on the Scottish Index of Multiple Deprivation (SIMD)",
                                                rep("", 11)
  ),
  "url" = c("https://www.gov.scot/publications/pupil-census-supplementary-statistics/",
            "https://www.gov.scot/publications/scottish-government-urban-rural-classification-2016/",
            "https://www.gov.scot/collections/scottish-index-of-multiple-deprivation-2020/",
            rep("", 11)
  ),
  check.names = FALSE
)



### 8 - Create ally_tables ----

# For each local authority 

las_workbooks <- list()

for (la in all_las) {
  table_1 <- formatted_data_las[[la]]$stage
  table_2 <- formatted_data_las[[la]]$sex
  table_3 <- formatted_data_las[[la]]$ethnic_group
  table_4 <- formatted_data_las[[la]]$urbrur6
  table_5 <- formatted_data_las[[la]]$simd
  table_6 <- formatted_data_las[[la]]$long_term_condition
  table_7 <- formatted_data_las[[la]]$care_for_someone  
  
  la_a11ytable <- 
    a11ytables::create_a11ytable(
      tab_titles = c("Cover", "Table of contents", "Notes", "Table_1", "Table_2", "Table_3", "Table_4", "Table_5", "Table_6", "Table_7"),
      sheet_types = c("cover", "contents", "notes", "tables", "tables", "tables", "tables", "tables", "tables", "tables"),
      sheet_titles = c(
        "Cover sheet",
        "Table of contents",
        "Notes",
        "Table 1: Percentage by stage",
        "Table 2: Percentage by sex [note 1]",
        "Table 3: Percentage by ethnic group [note 1]",
        "Table 4: Percentage by urban rural classification [note 1] [note 2]",
        "Table 5: Percentage by SIMD [note 1] [note 3]",
        "Table 6: Percentage by long term health condition [note 4]",
        "Table 7: Percentage by caring responsibilities [note 5]"
      ),
      custom_rows = list(
        NA_character_,
        NA_character_,
        NA_character_,
        "Some cells refer to notes which can be found on the notes worksheet.",
        "Some cells refer to notes which can be found on the notes worksheet.",
        "Some cells refer to notes which can be found on the notes worksheet.",
        "Some cells refer to notes which can be found on the notes worksheet.",
        "Some cells refer to notes which can be found on the notes worksheet.",
        "Some cells refer to notes which can be found on the notes worksheet.",
        "Some cells refer to notes which can be found on the notes worksheet."
      ),
      tables = list(cover_list, contents_la_df, notes_la_df, table_1, table_2, table_3, table_4, table_5, table_6, table_7)
    )
  
  las_workbooks[[la]] <- a11ytables::generate_workbook(la_a11ytable)
}


# For national

table_1 <- formatted_data_national$stage
table_2 <- formatted_data_national$la
table_3 <- formatted_data_national$sex
table_4 <- formatted_data_national$stage_and_sex
table_5 <- formatted_data_national$simd
table_6 <- formatted_data_national$stage_and_simd
table_7 <- formatted_data_national$urbrur6
table_8 <- formatted_data_national$stage_and_urbrur6
table_9 <- formatted_data_national$ethnic_group
table_10 <- formatted_data_national$stage_and_ethnic_group
table_11 <- formatted_data_national$asn
table_12 <- formatted_data_national$stage_and_asn
table_13 <- formatted_data_national$care_for_someone  
table_14 <- formatted_data_national$stage_and_care_for_someone  
table_15 <- formatted_data_national$long_term_condition
table_16 <- formatted_data_national$stage_and_long_term_condition

national_a11ytable <- 
  a11ytables::create_a11ytable(
    tab_titles <- c("Cover", "Table of contents", "Notes", paste("Table", 1:16, sep = "_")),
    sheet_types = c("cover", "contents", "notes", rep("tables", times = 16)),
    sheet_titles = c(
      "Cover sheet",
      "Table of contents",
      "Notes",
      "Table 1: Percentage by stage",
      "Table 2: Percentage by local authority [note 14]",
      "Table 3: Percentage by sex [note 1]",
      "Table 4: Percentage by stage and sex [note 1]",
      "Table 5: Percentage by SIMD [note 1] [note 3]",
      "Table 6: Percentage by stage and SIMD [note 1] [note 3]",
      "Table 7: Percentage by urban rural classification [note 1] [note 2]",
      "Table 8: Percentage by stage and urban rural classification [note 1] [note 2]",
      "Table 9: Percentage by ethnic group [note 1]",
      "Table 10: Percentage by stage and ethnic group [note 1]",
      "Table 11: Percentage by Additional Support Needs [note 1]",
      "Table 12: Percentage by stage and Additional Support Needs [note 1]",
      "Table 13: Percentage by caring responsibilities [note 5]",
      "Table 14: Percentage by stage and caring responsibilities [note 5]",
      "Table 15: Percentage by long term health condition [note 4]",
      "Table 16: Percentage by stage and long term health condition [note 4]"
    ),
    custom_rows = list(
      NA_character_,
      NA_character_,
      NA_character_,
      "Some cells refer to notes which can be found on the notes worksheet.",
      "Some cells refer to notes which can be found on the notes worksheet.",
      "Some cells refer to notes which can be found on the notes worksheet.",
      "Some cells refer to notes which can be found on the notes worksheet.",
      "Some cells refer to notes which can be found on the notes worksheet.",
      "Some cells refer to notes which can be found on the notes worksheet.",
      "Some cells refer to notes which can be found on the notes worksheet.",
      "Some cells refer to notes which can be found on the notes worksheet.",
      "Some cells refer to notes which can be found on the notes worksheet.",
      "Some cells refer to notes which can be found on the notes worksheet.",
      "Some cells refer to notes which can be found on the notes worksheet.",
      "Some cells refer to notes which can be found on the notes worksheet.",
      "Some cells refer to notes which can be found on the notes worksheet.",
      "Some cells refer to notes which can be found on the notes worksheet.",
      "Some cells refer to notes which can be found on the notes worksheet.",
      "Some cells refer to notes which can be found on the notes worksheet."
    ),
    tables = list(cover_list, contents_national_df, notes_national_df, table_1, table_2, table_3, table_4, table_5, table_6, table_7, 
                  table_8, table_9, table_10, table_11, table_12, table_13, table_14, table_15, table_16)
  )
  
national_workbook <- a11ytables::generate_workbook(national_a11ytable)



### 9 - Format hyperlinks in the Notes page ----

# We need a manual workaround to do this, rather than doing it in a11ytables because a11ytables has no functionality for embedding hyperlinks in dataframes, only lists
class(notes_la_df$url)<-"hyperlink" # mark as a hyperlink
class(notes_national_df$url)<-"hyperlink" # mark as a hyperlink

# Loop over each local authority
for (la in all_las) {
  writeData(las_workbooks[[la]], sheet = "Notes", notes_la_df$url,
            startCol = which(colnames(notes_la_df) == "Relevant hyperlinks (where appropriate)"), startRow = 4)
}

# For national
writeData(national_workbook, sheet = "Notes", notes_national_df$url,
          startCol = which(colnames(notes_national_df) == "Relevant hyperlinks (where appropriate)"), startRow = 4)


notes_la_df <- notes_la_df %>%
 select("Note number", "Note text", "Relevant hyperlinks (where appropriate)")

notes_national_df <- notes_national_df %>%
  select("Note number", "Note text", "Relevant hyperlinks (where appropriate)")

# Loop over each local authority
for (la in all_las) {
  writeData(las_workbooks[[la]], sheet = "Notes", notes_la_df,
            startRow = 4, colNames = FALSE) # Overwrite the sheet to get the new name overlaying the hyperlink
}

# For national
writeData(national_workbook, sheet = "Notes", notes_national_df,
          startRow = 4, colNames = FALSE) # Overwrite the sheet to get the new name overlaying the hyperlink



### 10 - Save outputs as an excel file to Merged folder ----

# For local authorities
# Saves the a11ytable as an excel sheet 
for (la in names(las_workbooks)) {
  file_name <- paste0(la, "_", year, "_final.xlsx")
  output_path <- here("output", year, la, "Suppressed and formatted", file_name)
  saveWorkbook(las_workbooks[[la]], output_path, overwrite = TRUE)
}

# For national
# Saves the a11ytable as an excel sheet
saveWorkbook(national_workbook, here("output", year, "National", "Suppressed and formatted", paste0("National_", year, "_final.xlsx")), overwrite = TRUE)



### END OF SCRIPT ###

