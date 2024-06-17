#########################################################################
# Name of file - 12n_npf_indicators.R
# Health and Wellbeing (HWB) Census 
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 4.2.2
#
# Description - Analyses, suppresses and formats data for NPF indicators
#########################################################################


### 0 - Setup ----

source(here::here("code", "00_setup.R"))
source(here::here("functions", "read_raw_data.R"))
source(here::here("functions", "analysis_one_characteristic.R"))
source(here::here("functions", "analysis_stage_and_characteristic.R"))
source(here::here("functions", "perform_analysis_national.R"))
source(here::here("functions", "perform_analysis_local_authority.R"))
source(here::here("functions", "perform_data_suppression.R"))
source(here::here("functions", "perform_formatting.R"))



### 1 - Read in raw data ----

hwb_analysis <- read_xlsx(file.path(raw_data_folder, year, "Merged", "09_joined_stages.xlsx"), sheet = 1)



### 2 - Set row order of response categories ----

cat_order_1 <- c("Three or more", 
                 "Two", 
                 "One",
                 "None",
                 "Prefer not to say")

cat_order_2 <- c("Strongly agree or Agree",
                 "Neither agree nor disagree",
                 "Strongly disagree or Disagree",
                 "Prefer not to say")

cat_order_3 <- c("Often or All of the time",
                 "Some of the time",
                 "Rarely or None of the time")

cat_order_4 <- c("Participated in leisure activity",
                 "Did not participate")

cat_order_5 <- c("Close to average",
                 "Slightly raised, High, or Very high")



### 3 - Replace response values as per Measures for Inclusion in publication document ----

# Aggregate "Strongly agree" and "Agree" and aggregate "Strongly disagree" and "Disagree"
hwb_analysis[hwb_analysis == "Strongly agree"] <- "Strongly agree or Agree"
hwb_analysis[hwb_analysis == "Agree"] <- "Strongly agree or Agree"
hwb_analysis[hwb_analysis == "Strongly disagree"] <- "Strongly disagree or Disagree"
hwb_analysis[hwb_analysis == "Disagree"] <- "Strongly disagree or Disagree"


# Combine columns wemwbs_10_confident and frequency_feeling_confident into one column called 'How often have you been feeling confident?'
# This is because the question was asked differently of different stages (frequency_feeling_confident P5-S1 and wemwbs_10_confident S2-S6)
hwb_analysis$'How often have you been feeling confident?' <- ifelse(hwb_analysis$pc_stage %in% c("P5", "P6", "P7", "S1"),
                                                                    hwb_analysis$frequency_feeling_confident,
                                                                    hwb_analysis$wemwbs_10_confident)

hwb_analysis$'How often have you been feeling confident?'[hwb_analysis$'How often have you been feeling confident?' == "Often"] <- "Often or All of the time"
hwb_analysis$'How often have you been feeling confident?'[hwb_analysis$'How often have you been feeling confident?' == "All of the time"] <- "Often or All of the time"
hwb_analysis$'How often have you been feeling confident?'[hwb_analysis$'How often have you been feeling confident?' == "Rarely"] <- "Rarely or None of the time"
hwb_analysis$'How often have you been feeling confident?'[hwb_analysis$'How often have you been feeling confident?' == "None of the time"] <- "Rarely or None of the time"



### 4 - Create derived variable for 'Participated in a positive leisure activity this year' ----

# Extract columns that begin with "positive_activities"
positive_activities_cols <- grep("^positive_activities", names(hwb_analysis), value = TRUE)

hwb_analysis$'Participated in a positive leisure activity this year' <- apply(hwb_analysis[positive_activities_cols], 1, function(row) {
  if (all(row == "Question not asked of stage")) {
    return("Question not asked of stage")
  } else if (all(is.na(row))) {
    return(NA)
  } else if (all(row == "Data not collected")) {
    return("Data not collected")
  } else if (all(row == "No")) {
    return(NA)
  } else if ("Yes" %in% row[setdiff(names(row), "positive_activities_none_of_above")]) {
    return("Participated in leisure activity")
  } else {
    return("Did not participate")
  }
})



### 5 - Create derived variable for SDQ scores ----

# Identify columns that start with "sdq"
sdq_columns <- grep("^sdq", names(hwb_analysis), value = TRUE)

# Identify columns that are recoded with 0 to "Certainly true" and 2 to "Not true" (Qs 7,11,14,21,25)
sdq_columns_a <- c("sdq_7_do_as_told", "sdq_11_good_friend", "sdq_14_people_like_me", "sdq_21_think_before_doing", "sdq_25_good_attention")

# Identify columns that are recoded with 2 to "Certainly true" and 0 to "Not true" (All other Qs)
sdq_columns_b <- setdiff(sdq_columns, sdq_columns_a)

# Replace values in SDQ columns with numeric scores based on conditions
hwb_analysis <- hwb_analysis %>%
  mutate(across(all_of(sdq_columns_a), ~case_when(
    . == "Certainly true" ~ 0,
    . == "Somewhat true" ~ 1,
    . == "Not true" ~ 2,
    TRUE ~ NA_real_
  )))

hwb_analysis <- hwb_analysis %>%
  mutate(across(all_of(sdq_columns_b), ~case_when(
    . == "Certainly true" ~ 2,
    . == "Somewhat true" ~ 1,
    . == "Not true" ~ 0,
    TRUE ~ NA_real_
  )))

# Convert columns to numeric
hwb_analysis[sdq_columns] <- lapply(hwb_analysis[sdq_columns], as.numeric)

# Calculate individual scales
hwb_analysis <- hwb_analysis %>% 
  mutate("emotional" = sdq_3_pains + sdq_8_worry_a_lot + sdq_13_often_unhappy + sdq_16_nervous_in_situations + sdq_24_fears) %>% 
  mutate("conduct" = sdq_5_lose_temper + sdq_7_do_as_told + sdq_12_fight_a_lot + sdq_18_accused_of_lying + sdq_22_taking_things) %>% 
  mutate("hyperactivity" = sdq_2_restless + sdq_10_fidgeting + sdq_15_easily_distracted + sdq_21_think_before_doing + sdq_25_good_attention) %>%
  mutate("peer" = sdq_6_usually_alone + sdq_11_good_friend + sdq_14_people_like_me + sdq_19_bullied + sdq_23_get_on_adults_more) %>%
  mutate("prosocial" = sdq_1_nice_to_others + sdq_4_share_with_others + sdq_9_helping_others + sdq_17_kind_to_younger_children + sdq_20_volunteering)

# Calculate total difficulties score
hwb_analysis <- hwb_analysis %>% 
  mutate("total_difficulties" = emotional + conduct + hyperactivity + peer)

# Total Difficulties score
# Newer 4-band categorisation
hwb_analysis$'Total difficulties score % - four band scale' <- ifelse(
  hwb_analysis$total_difficulties <= 14, 
  'Close to average',
  ifelse(
    hwb_analysis$total_difficulties >= 15 & hwb_analysis$total_difficulties <= 40,
    'Slightly raised, High, or Very high',
    NA
  )
)



### 6 - Aggregate up EthnicBackground column ----

# Define lookup table
lookup_ethnic_background <- c("White - Scottish" = "White - Scottish",
                              "African - African / Scottish / British" = "African / Black / Caribbean",
                              "Caribbean or Black - Caribbean / British / Scottish" = "African / Black / Caribbean",
                              "Asian - Indian/British/Scottish" = "Asian",
                              "Asian - Pakistani / British / Scottish" = "Asian",
                              "Asian - Bangladeshi / British / Scottish" = "Asian",
                              "Asian - Chinese / British / Scottish" = "Asian",
                              "White - Other" = "White - Other",
                              "Not Disclosed" = "Other groups or not known",
                              "Mixed or multiple ethnic groups" = "Mixed or multiple ethnic groups",
                              "Asian - Other" = "Asian",
                              "White - Gypsy/Traveller" = "White - Other",
                              "White - Other British" = "White - Other British",
                              "White - Irish" = "White - Other",
                              "White - Polish" = "White - Other",
                              "Caribbean or Black - Other" = "African / Black / Caribbean",
                              "African - Other" = "African / Black / Caribbean",
                              "Other - Arab" = "Other groups or not known",
                              "Not known" = "Other groups or not known",
                              "Other - Other" = "Other groups or not known",
                              "Not known" = "Other groups or not known")

# Apply lookup table to hwb_analysis to rename variables in column "EthnicBackground"
hwb_analysis$EthnicBackground <- lookup_ethnic_background[hwb_analysis$EthnicBackground]



### 7 - Define variables for analysis ----

variables <- data.frame(
  variable = c("number_close_friends",
               "friends_treat_me_well",
               "adults_taking_into_account",
               "How often have you been feeling confident?",
               "Participated in a positive leisure activity this year",
               "I_will_be_ok",
               "Total difficulties score % - four band scale"),
  cat_order = c("cat_order_1",
                "cat_order_2",
                "cat_order_2",
                "cat_order_3",
                "cat_order_4",
                "cat_order_2",
                "cat_order_5")
)



### 8 - Perform analysis on selected variables ----

# For national
national_npf <- perform_analysis_national(hwb_analysis, variables)



### 9 - Remove tibbles which aren't needed ----

# Select only the tibbles required
national_npf <- national_npf[c("stage", "sex", "simd", "urbrur6", "ethnic_group", "care_for_someone", "long_term_condition")]

# Add a new column 'Survey topic' at the start of each tibble with the constant value "NPF indicators"
national_npf <- map(national_npf, ~ {
  # Add the new column
  .x <- .x %>%
    mutate(`Survey topic` = "NPF indicators")
  # Reorder columns to make 'Survey topic' the first column
  .x <- .x %>%
    select(`Survey topic`, everything())
  return(.x)
})



### 10 - Read in survey questions for publication metadata ----

# This is used to replace values in the 'Survey question' column when formatting
survey_question_metadata <-
  read_xlsx(here("metadata",
                 year,
                 "hwb_metadata_survey_questions_for_publication.xlsx"))

# Remove text matching the pattern " [note X]" from the survey_question column
survey_question_metadata$survey_question <- gsub("\\s*\\[note \\d+\\]", "", survey_question_metadata$survey_question)



### 11 - Perform data suppression ----

# For national
national_npf_suppressed <- perform_data_suppression(national_npf)



### 12 - Format data as dataframes ----

# For national
national_npf_formatted <- perform_formatting(national_npf_suppressed)



### 13 - Save outputs as an excel file to Merged folder ----

# Save national
write_xlsx(
  national_npf_formatted,
  here("output", year, "National", "Suppressed and formatted", paste0(year, "_npf_indicators.xlsx"))
)



### END OF SCRIPT ###

