#########################################################################
# Name of file - 13b_formatting_and_suppression_su.R
# Health and Wellbeing (HWB) Census 
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 4.2.2
#
# Description - Formats and suppresses tables for publication for substance use data
#########################################################################



### 0 - Setup ----

source(here::here("code", "00_setup.R"))
source(here::here("functions", "read_raw_data.R"))
source(here::here("functions", "perform_data_suppression.R"))
source(here::here("functions", "perform_formatting.R"))
source(here::here("functions", "reorder_columns.R"))



### 1 - Read in raw data ----

# Define function to read in substance use files
read_excel_files <- function(directory) {
  excel_files <- list.files(path = directory, pattern = "\\.xlsx$", full.names = TRUE)
  raw_data <- list()
  
  for (file in excel_files) {
    if (grepl("substance_use", file)) {  # Check if the file name contains "substance_use"
      cat("Reading file:", file, "\n")
      
      file_name <- tools::file_path_sans_ext(basename(file))
      sheets <- readxl::excel_sheets(file)
      sheet_data <- list()
      
      for (sheet in sheets) {
        # Read each sheet directly as a tibble and store it in the list with sheet name as key
        sheet_data[[sheet]] <- readxl::read_excel(file, sheet = sheet)
      }
      
      raw_data[[file_name]] <- sheet_data
    } else {
      cat("Skipping file not containing 'substance_use':", file, "\n")
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
                 "hwb_metadata_survey_questions_for_publication_su.xlsx"))



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


# Function to replace NA with "-" or "0" based on condition
replace_NA <- function(df) {
  df[] <- lapply(df, function(x) {
    ifelse(is.na(x) & df$Response == "Total", "0", ifelse(is.na(x), "-", x))
  })
  return(df)
}

# Applying the function to each dataframe in each list
combined_data_las <- lapply(combined_data_las, function(lst) {
  lapply(lst, replace_NA)
})



### 4 - Perform data suppression ----

# For local authorities and national
las_suppressed <- lapply(combined_data_las, perform_data_suppression)



### 5 - Format data as dataframes ----

# For local authorities and national
formatted_data_las <- lapply(las_suppressed, perform_formatting)



### 6 - Re-order columns ----

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
  "Note number" = paste0("[note ", 1:26, "]"),
  "Note text" = c("Based on Scottish Index of Multiple Deprivation (SIMD) SIMD 2020. For more information see the Scottish Government's website.",
                  "Scottish Index of Multiple Deprivation (SIMD) quintiles were calculated using postcode data collected in the survey.", 
                  "The local authority East Renfrewshire Council did not collect data on sex.",
                  "The local authority East Renfrewshire Council, Falkirk Council and Perth and Kinross Council did not collect data on SIMD.",
                  "For the S4 Substance Use Survey, \"Unknown\" respondents were those that skipped the question \"What is your sex?\" or the local authority did not collect data for this question item.",
                  "For the S4 Substance Use Survey, \"Unknown\" respondents were those that skipped the question \"Please enter your SIMD value\" or the local authority did not collect data for this question item.",
                  "Pupils were classified as ‘regular smokers’ (defined as usually smoking at least one cigarette a week), ‘occasional smokers’ (currently smoking, but less than one cigarette a week) or ‘non-smokers’ (pupils who had never smoked or were not current smokers).",
                  "The responses show the percentage of those who ever had a proper alcoholic drink by type of alcohol.",
                  "The percentages are based on those who responded \"Yes\"  to Q17 \"Have you ever taken illegal drugs, drugs formerly known as legal highs, solvents or prescription drugs that were not prescribed to you?\" AND who selected \"I take drugs once or twice a month\" or \"I take drugs at least once a week or more\" to Q18 \"How often do you use drugs?\". Used drugs in the last month is calculated as those who have taken drugs once or twice a month or taken drugs at least once a week or more.",
                  "This is a multi response question (select all that apply). Percentages will not sum to 100.",
                  "Asked of S2 and S4. This is a derived variable from the Smoking Status question \"How often do you smoke tobacco at present? Every day / At least once a week, but not every day / Less than once a week / I do not smoke / Prefer not to say\".\n Respondents are classified as:\n 'regular smokers': ‘every day’ OR ‘at least once a week but not every day’\n 'occasional smokers':  ‘less than once a week’\n 'non-smokers' (respondents who had never smoked or who were not current smokers):  ‘I do not smoke’.",
                  "For the Stage Questionnaire, the characteristics are attached by linking the HWB Census data with the Pupil Census 2021. Only records with a unique valid Scottish Candidate Number (SCN) can be linked. Where SCNs were not collected or where characteristics data was not available in the Pupil Census, records were included as \"Not known\".",
                  "The urban/rural classifications in Pupil Census are based on the 2016 urban rural classification.",
                  "This is a self identified caring responsibility. Caring responsibilities are asked in the stage questionnaires for P7 - S6. The question asks \"Do you care for, or look after, someone? For example, because they have a disability, an illness, a drug or alcohol problem, a mental health problem, or problems related to old age.\" As this is self reported, it may differ from other estimates of the number of young carers such as pupils identified as young carers in school management information system, eligibility for the Young Carers Grant or Young Carers Package, or estimates from other data collections for example the Scottish Health Survey.",
                  "This is a self identified long term health condition question. This question is asked of P5 - S6. The question asks \"Do you have a physical or mental health condition or illness lasting or expected to last 12 months or more?\"",
                  "Respondents are classified as:\n 'regular vapers': response to frequency of use question is‘every day’ OR ‘at least once a week but not every day’\n 'occasional vapers': response to frequency of use question is ‘less than once a week’ or \n 'non-vapers' (respondents who had never used an e-cigarette or no longer use an e-cigarette): response to frequency of use question is ‘I do not smoke’.",
                  "Response options are Taking part in a lottery for example National Lottery Lotto (the main National lottery draw), Health Lottery, Postcode Lottery, Scratchcards, Euromillions, Thunderball, Hotpicks / Personally placing a bet at a betting shop for example visiting a bookies to bet on football or horse racing / Gambling websites or apps where you can win real money or other prizes for example poker, casino games, bingo, betting on sport or racing / Fruit machines (puggies, slot machines) at an arcade, pub or club / Private betting with friends for example playing cards or placing a private bet for money on the outcome of an event / Bingo at a bingo club or somewhere else, for example social club, holiday park / Visiting a betting shop to play gaming machines / Visiting a casino to play casino games / Any other type of gambling.",
                  "Questions on use of e-cigarettes were asked in the S2 and S4 questionnaires. The frequency of use of e-cigarettes question asked \"How often do you use e-cigarettes / vape at present? Every day / At least once a week, but not every day / Less than once a week / I do not use e-cigarettes or vape / Prefer not to say.\"",
                  "Questions on the use of e-cigarettes were asked in the S4 Substance Use survey. The frequency of smoking e-cigarettes question asked \"Now read the following statements carefully and tick the box next to the ONE which best describes you. I have never used an e-cigarette or vape / I used to use e-cigarettes or vapes but don't use them anymore / I have tried an e-cigarette or vape once / I have tried e-cigarettes or vapes a few times / I use e-cigarettes or vapes sometimes, but no more than once a month / I use e-cigarettes or vapes once a week or more / Prefer not to say\".",
                  "Questions on use of cigarettes were asked in the S2 and S4 questionnaires. The frequency of use of cigarettes question asked \"How often do you smoke tobacco at present? Every day / At least once a week, but not every day / Less than once a week / I do not smoke / Prefer not to say\"",
                  "Questions on the use of cigarettes were asked in the S4 substance use questionnaire. The frequency of use of cigarettes question asked \"Now read the following statements carefully and select the option which best describes you: I have never smoked / I have only ever tried smoking once / I used to smoke sometimes but I never smoke a cigarette now / I sometimes smoke cigarettes now but I don't smoke as many as one a week / I usually smoke between one and six cigarettes a week / I usually smoke more than six cigarettes a week / Prefer not to say\"",
                  "Questions on alcohol use were asked in the S2 and S4 questionnaires \"How often do you USUALLY have an alcoholic drink? More than once a week / About once a week / About once a fortnight / About once a month / Only a few times a year / I never drink alcohol now\".",
                  "Use of e-cigarettes at present aggregates regular vapers and occasional vapers. This is: response to frequency of use question is  ‘every day’ OR ‘at least once a week but not every day’ OR 'less than once a week’.",
                  "The percentages are based on those who responded \"Yes\"  to Q17 \"Have you ever taken illegal drugs, drugs formerly known as legal highs, solvents or prescription drugs that were not prescribed to you?\" (although the number of respondents may be fewer if a respondent answered \"Yes\" to that question, but did not answer this question).",
                  "This question is aggregated from the question \"How often do you use drugs?\". The number of respondents is the same for the aggregated and unagreggated measures.",
                  "Percentages will not sum to 100%."
  ),
  "Relevant hyperlinks (where appropriate)" = c("More information on the Scottish Index of Multiple Deprivation (SIMD)",
                                                rep("", 10),
                                                "More information on the Pupil Census",
                                                "More information on the urban rural classification 2016",
                                                rep("", 13)
  ),
  "url" = c("https://www.gov.scot/collections/scottish-index-of-multiple-deprivation-2020/",
            rep("", 10),
            "https://www.gov.scot/publications/pupil-census-supplementary-statistics/",
            "https://www.gov.scot/publications/scottish-government-urban-rural-classification-2016/",
            rep("", 13)
  ),
  check.names = FALSE
)

notes_national_df <- data.frame(
  "Note number" = paste0("[note ", 1:23, "]"),
  "Note text" = c("Based on Scottish Index of Multiple Deprivation (SIMD) SIMD 2020. For more information see the Scottish Government's website.",
                  "Scottish Index of Multiple Deprivation (SIMD) quintiles were calculated using postcode data collected in the survey.",
                  "The local authority East Renfrewshire Council did not collect data on sex.",
                  "The local authority East Renfrewshire Council, Falkirk Council and Perth and Kinross Council did not collect data on SIMD.",
                  "\"Unknown\" respondents were those that skipped the question \"What is your sex?\" or the local authority did not collect data for this question item.",
                  "\"Unknown\" respondents were those that skipped the question \"Please enter your SIMD value\" or the local authority did not collect data for this question item.",
                  "Pupils were classified as ‘regular smokers’ (defined as usually smoking at least one cigarette a week), ‘occasional smokers’ (currently smoking, but less than one cigarette a week) or ‘non-smokers’ (pupils who had never smoked or were not current smokers).",
                  "The responses to Q11 show the percentage of those who ever had a proper alcoholic drink by type of alcohol.",
                  "The percentages are based on those who responded \"Yes\"  to Q17 \"Have you ever taken illegal drugs, drugs formerly known as legal highs, solvents or prescription drugs that were not prescribed to you?\" AND who selected \"I take drugs once or twice a month\" or \"I take drugs at least once a week or more\" to Q18 \"How often do you use drugs?\". Used drugs in the last month is calculated as those who have taken drugs once or twice a month or taken drugs at least once a week or more.",
                  "This is a multi response question (select all that apply). Percentages will not sum to 100.",
                  "Asked of S2 and S4. This is a derived variable from the Smoking Status question \"How often do you smoke tobacco at present? Every day / At least once a week, but not every day / Less than once a week / I do not smoke / Prefer not to say\".\n Respondents are classified as:\n 'regular smokers': ‘every day’ OR ‘at least once a week but not every day’\n 'occasional smokers': ‘less than once a week’\n 'non-smokers' (respondents who had never smoked or who were not current smokers): ‘I do not smoke’.",
                  "The characteristics are attached by linking the HWB Census data with the Pupil Census 2021. Only records with a unique valid Scottish Candidate Number (SCN) can be linked. Where SCNs were not collected or where characteristics data was not available in the Pupil Census, records were included as \"Not known\".",
                  "The urban/rural classifications in Pupil Census are based on the 2016 urban rural classification.",
                  "This is a self identified caring responsibility. Caring responsibilities are asked in the stage questionnaires for P7 - S6. The question asks \"Do you care for, or look after, someone?  For example, because they have a disability, an illness, a drug or alcohol problem, a mental health problem, or problems related to old age.\"",
                  "This is a self identified long term health condition question. This question is asked of P5 - S6. The question asks \"Do you have a physical or mental health condition or illness lasting or expected to last 12 months or more?\"",
                  "Respondents are classified as:\n 'regular vapers': response to frequency of use question is ‘every day’ OR ‘at least once a week but not every day’\n 'occasional vapers': response to frequency of use question is ‘less than once a week’ or\n 'non-vapers' (respondents who had never used an e-cigarette or no longer use an e-cigarette): response to frequency of use question is ‘I do not smoke’.",
                  "Response options are Taking part in a lottery for example National Lottery Lotto (the main National lottery draw), Health Lottery, Postcode Lottery, Scratchcards, Euromillions, Thunderball, Hotpicks / Personally placing a bet at a betting shop for example visiting a bookies to bet on football or horse racing / Gambling websites or apps where you can win real money or other prizes for example poker, casino games, bingo, betting on sport or racing, Fruit machines (puggies, slot machines) at an arcade, pub or club / Private betting with friends for example playing cards or placing a private bet for money on the outcome of an event / Bingo at a bingo club or somewhere else, for example social club, holiday park / Visiting a betting shop to play gaming machines / Visiting a casino to play casino games / Any other type of gambling.",
                  "Questions on use of e-cigarettes were asked in the S2 and S4 questionnaires. The frequency of use of e-cigarettes question asked \"How often do you use e-cigarettes / vape at present? Every day / At least once a week, but not every day / Less than once a week / I do not use e-cigarettes or vape / Prefer not to say.\"",
                  "Questions on the use of e-cigarettes were asked in the S4 Substance Use survey. The frequency of smoking question asked \"Now read the following statements carefully and tick the box next to the ONE which best describes you. I have never used an e-cigarette or vape / I used to use e-cigarettes or vapes but don't use them anymore / I have tried an e-cigarette or vape once / I have tried e-cigarettes or vapes a few times / I use e-cigarettes or vapes sometimes, but no more than once a month / I use e-cigarettes or vapes once a week or more / Prefer not to say\".",
                  "Questions on use of cigarettes were asked in the S2 and S4 questionnaires. The frequency of use of cigarettes question asked \"How often do you smoke tobacco at present? Every day / At least once a week, but not every day / Less than once a week / I do not smoke / Prefer not to say\"",
                  "Questions on the use of cigarettes were asked in the S4 substance use questionnaire. The frequency of use of cigarettes question asked \"Now read the following statements carefully and select the option which best describes you: I have never smoked / I have only ever tried smoking once / I used to smoke sometimes but I never smoke a cigarette now / I sometimes smoke cigarettes now but I don't smoke as many as one a week / I usually smoke between one and six cigarettes a week / I usually smoke more than six cigarettes a week / Prefer not to say\".",
                  "Questions on alcohol use were asked in the S2 and S4 questionnaires \"How often do you USUALLY have an alcoholic drink? More than once a week / About once a week / About once a fortnight / About once a month / Only a few times a year / I never drink alcohol now\".",
                  "Use of e-cigarettes at present aggregates regular vapers and occasional vapers. This is: response to frequency of use question is  ‘every day’ OR ‘at least once a week but not every day’ OR 'less than once a week’."
  ),
  "Relevant hyperlinks (where appropriate)" = c("More information on the Scottish Index of Multiple Deprivation (SIMD)",
                                                rep("", 10),
                                                "More information on the Pupil Census",
                                                "More information on the urban rural classification 2016",
                                                rep("", 10)
  ),
  "url" = c("https://www.gov.scot/collections/scottish-index-of-multiple-deprivation-2020/",
            rep("", 10),
            "https://www.gov.scot/publications/pupil-census-supplementary-statistics/",
            "https://www.gov.scot/publications/scottish-government-urban-rural-classification-2016/",
            rep("", 10)
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
        "Table 2: Percentage by sex [note 5] [note 12]",
        "Table 3: Percentage by ethnic group [note 12]",
        "Table 4: Percentage by urban rural classification [note 12] [note 13]",
        "Table 5: Percentage by SIMD [note 1] [note 2] [note 6] [note 12]",
        "Table 6: Percentage by long term health condition [note 15]",
        "Table 7: Percentage by caring responsibilities [note 14]"
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
## Resume here monday
table_1 <- formatted_data_las$National$stage
table_2 <- formatted_data_las$National$la
table_3 <- formatted_data_las$National$sex
table_4 <- formatted_data_las$National$stage_and_sex
table_5 <- formatted_data_las$National$simd
table_6 <- formatted_data_las$National$stage_and_simd
table_7 <- formatted_data_las$National$urbrur6
table_8 <- formatted_data_las$National$stage_and_urbrur6
table_9 <- formatted_data_las$National$ethnic_group
table_10 <- formatted_data_las$National$stage_and_ethnic_group
table_11 <- formatted_data_las$National$asn
table_12 <- formatted_data_las$National$stage_and_asn
table_13 <- formatted_data_las$National$care_for_someone  
table_14 <- formatted_data_las$National$stage_and_care_for_someone  
table_15 <- formatted_data_las$National$long_term_condition
table_16 <- formatted_data_las$National$stage_and_long_term_condition

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
  file_name <- paste0(la, "_", year, "_final_substance_use.xlsx")
  output_path <- here("output", year, la, "Suppressed and formatted", file_name)
  saveWorkbook(las_workbooks[[la]], output_path, overwrite = TRUE)
}

# For national
# Saves the a11ytable as an excel sheet
saveWorkbook(national_workbook, here("output", year, "National", "Suppressed and formatted", paste0("National_", year, "_final_substance_use.xlsx")), overwrite = TRUE)


### END OF SCRIPT ###
