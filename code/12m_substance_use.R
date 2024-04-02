#########################################################################
# Name of file - 12m_substance_use.R
# Health and Wellbeing (HWB) Census 
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 4.2.2
#
# Description - Analyses data for substance use from both the S4 substance use survey and the S2 & S4 stage questionnaires
#########################################################################


### 0 - Setup ----

source(here::here("code", "00_setup.R"))
source(here::here("functions", "read_raw_data.R"))
source(here::here("functions", "analysis_one_characteristic.R"))
source(here::here("functions", "analysis_stage_and_characteristic.R"))
source(here::here("functions", "perform_analysis_national.R"))
source(here::here("functions", "perform_analysis_local_authority.R"))



### 1 - Read in raw data ----

# Read in HWB data
hwb_analysis <- read_xlsx(file.path(raw_data_folder, year, "Merged", "09_joined_stages.xlsx"), sheet = 1)

# Read in substance use data
su_analysis <- read_xlsx(file.path(raw_data_folder, year, "Merged", "07_validated_school_names_su.xlsx"), sheet = 1)

# Insert pc_stage column in su_analysis so we can compare stages
su_analysis <- cbind(pc_stage = "S4", su_analysis)

# Insert pc_la column into su_analysis, populated with the same values as hwb_la (this is because analysis is done on pc_la, i.e. the local authority a participant was resident
# in when completing the pupil census, rather than the HWB census)
su_analysis$pc_la <- su_analysis$hwb_la

# Insert EthnicBackground, UrbRur6, asn, care_for_someone, long_term_condition columns into su_analysis, populated with "Not known"
su_analysis <- data.frame(
  EthnicBackground = rep("Not known", nrow(su_analysis)),
  UrbRur6 = rep("Not known", nrow(su_analysis)),
  asn = rep("Not known", nrow(su_analysis)),
  care_for_someone = rep("Not known", nrow(su_analysis)),
  long_term_condition = rep("Not known", nrow(su_analysis)),
  su_analysis
)

# Rename sex to Gender and simd_decile to SIMD2020v2_Quintile in su_analysis
names(su_analysis)[names(su_analysis) == "sex"] <- "Gender"
names(su_analysis)[names(su_analysis) == "simd_decile"] <- "SIMD2020v2_Quintile"

# Apply simd lookup to su_analysis
source(here::here("lookups", "lookup_simd.R"))
su_analysis$SIMD2020v2_Quintile <- lookup_simd[su_analysis$SIMD2020v2_Quintile]

# In su_analysis column Gender, replace "Data not collected" and NA with "Not known"
su_analysis$Gender <- ifelse(is.na(su_analysis$Gender) | su_analysis$Gender == "Data not collected", "Not known", su_analysis$Gender)

# In su_analysis column SIMD2020v2_Quintile, replace NA with "Not known"
su_analysis$SIMD2020v2_Quintile <- ifelse(is.na(su_analysis$SIMD2020v2_Quintile), "Not known", su_analysis$SIMD2020v2_Quintile)



### 2 - Set row order of response categories ----

cat_order_1 <- c("Regular smokers",
                 "Occasional smokers",
                 "Non-smokers",
                 "Prefer not to say")

cat_order_2 <- c("Regular vapers",
                 "Occasional vapers",
                 "Non-vapers",
                 "Prefer not to say")

cat_order_3 <- c("Use of e-cigarettes at present",
                 "Non-vapers",
                 "Prefer not to say")

cat_order_4 <- c("I have never used an e-cigarette / vape",
                 "I used to use e-cigarettes / vapes but don't use them anymore",
                 "I have tried an e-cigarette / vape once",
                 "I have tried e-cigarettes / vapes a few times",
                 "I use e-cigarettes / vapes sometimes, but no more than once a month",
                 "I use e-cigarettes / vapes once a week or more",
                 "Prefer not to say")

cat_order_5 <- c("I have tried e-cigarettes once or more.",
                 "I use e-cigarettes at least once a week (regular vaper).")

cat_order_6 <- c("I am a non-smoker of tobacco but have used e-cigarettes at least once.",
                 "I am a non-smoker of tobacco but use e-cigarettes at least once a week (regular vaper).")

cat_order_7 <- c("More than once a week",
                 "About once a week",
                 "About once a fortnight",
                 "About once a month",
                 "Only a few times a year",
                 "I never drink alcohol now")

cat_order_8 <- c("I buy it in a pub or bar",
                 "I buy it in a club or disco",
                 "I buy it from an off-licence",
                 "I buy it from a shop",
                 "I buy it from a supermarket",
                 "I buy it from a website / online / internet",
                 "I get it from a friend",
                 "I get it from a relative",
                 "From home (either with or without permission)",
                 "Some other way",
                 "Prefer not to say")

cat_order_9 <- c("Yes",
                 "No",
                 "Prefer not to say")

cat_order_10 <- c("I have only taken drugs once",
                 "I used to take drugs sometimes but I don't take them anymore",
                 "I take drugs a few times a year",
                 "I take drugs once or twice a month",
                 "I take drugs at least once a week or more",
                 "Prefer not to say")

cat_order_11 <- c("Spent their own money on any gambling activities",
                 "Never spent their own money on any gambling activities")



### 3 - Replace response values as per Measures for Inclusion in publication document ----

hwb_analysis$frequency_smoking[hwb_analysis$frequency_smoking == "Every day"] <- "Regular smokers"
hwb_analysis$frequency_smoking[hwb_analysis$frequency_smoking == "At least once a week, but not every day"] <- "Regular smokers"
hwb_analysis$frequency_smoking[hwb_analysis$frequency_smoking == "Less than once a week"] <- "Occasional smokers"
hwb_analysis$frequency_smoking[hwb_analysis$frequency_smoking == "I do not smoke"] <- "Non-smokers"

su_analysis$cigarettes_smoking_status[su_analysis$cigarettes_smoking_status == "I usually smoke more than six cigarettes a week"] <- "Regular smokers"
su_analysis$cigarettes_smoking_status[su_analysis$cigarettes_smoking_status == "I usually smoke between one and six cigarettes a week"] <- "Regular smokers"
su_analysis$cigarettes_smoking_status[su_analysis$cigarettes_smoking_status == "I sometimes smoke cigarettes now but I don't smoke as many as one a week"] <- "Occasional smokers"
su_analysis$cigarettes_smoking_status[su_analysis$cigarettes_smoking_status == "I used to smoke sometimes but I never smoke a cigarette now"] <- "Non-smokers"
su_analysis$cigarettes_smoking_status[su_analysis$cigarettes_smoking_status == "I have only ever tried smoking once"] <- "Non-smokers"
su_analysis$cigarettes_smoking_status[su_analysis$cigarettes_smoking_status == "I have never smoked"] <- "Non-smokers"

hwb_analysis$frequency_e_cigarettes[hwb_analysis$frequency_e_cigarettes == "Every day"] <- "Regular vapers"
hwb_analysis$frequency_e_cigarettes[hwb_analysis$frequency_e_cigarettes == "At least once a week, but not every day"] <- "Regular vapers"
hwb_analysis$frequency_e_cigarettes[hwb_analysis$frequency_e_cigarettes == "Less than once a week"] <- "Occasional vapers"
hwb_analysis$frequency_e_cigarettes[hwb_analysis$frequency_e_cigarettes == "I do not smoke"] <- "Non-vapers"

su_analysis$alcohol_frequency_type_beer[su_analysis$alcohol_frequency_type_beer == "Every day" | su_analysis$alcohol_frequency_type_beer == "Every week"] <- "Beer or lager"
su_analysis$alcohol_frequency_type_wine[su_analysis$alcohol_frequency_type_wine == "Every day" | su_analysis$alcohol_frequency_type_wine == "Every week"] <- "Wine or champagne"
su_analysis$alcohol_frequency_type_alcopops[su_analysis$alcohol_frequency_type_alcopops == "Every day" | su_analysis$alcohol_frequency_type_alcopops == "Every week"] <- "Alcopops (e.g. Smirnoff Ice, Bacardi Breezer, WKD)"
su_analysis$alcohol_frequency_type_spirits[su_analysis$alcohol_frequency_type_spirits == "Every day" | su_analysis$alcohol_frequency_type_spirits == "Every week"] <- "Spirits (e.g. whisky, vodka, rum)"
su_analysis$alcohol_frequency_type_cider[su_analysis$alcohol_frequency_type_cider == "Every day" | su_analysis$alcohol_frequency_type_cider == "Every week"] <- "Cider"
su_analysis$alcohol_frequency_type_wine_fortified[su_analysis$alcohol_frequency_type_wine_fortified == "Every day" | su_analysis$alcohol_frequency_type_wine_fortified == "Every week"] <- "Fortified (strong) wine (e.g. sherry, martini, port, Buckfast)"
su_analysis$alcohol_frequency_type_other[su_analysis$alcohol_frequency_type_other == "Every day" | su_analysis$alcohol_frequency_type_other == "Every week"] <- "Any other drink that contains alcohol"



### 4 - Create aggregated and derived measures ----

# How do you usually get your cigarettes/tobacco (derived measure)
# As this is a tick-box question with participants allowed to select multiple answers, we create multiple columns
su_analysis <- su_analysis %>%
  mutate(cigarettes_source_shops = case_when(
    (
      cigarettes_source_supermarket == "Yes" |
        cigarettes_source_newsagent == "Yes" |
        cigarettes_source_garage_shop == "Yes" |
        cigarettes_source_van == "Yes" |
        cigarettes_source_hop_other == "Yes"
    )  ~ "Yes",
    (
      is.na(cigarettes_source_supermarket) &
        is.na(cigarettes_source_newsagent) &
        is.na(cigarettes_source_garage_shop) &
        is.na(cigarettes_source_van) &
        is.na(cigarettes_source_hop_other)
    )  ~ "Not applicable",
    TRUE ~ "No"
  )) %>%
  mutate(cigarettes_source_friends_relatives_someone_else = case_when(
    (
      cigarettes_source_friends_relatives == "Yes" |
        cigarettes_source_someone_else == "Yes"
    ) ~ "Yes",
    (
      is.na(cigarettes_source_friends_relatives) &
        is.na(cigarettes_source_someone_else)
    ) ~ "Not applicable",
    TRUE ~ "No"
  )) %>%
  mutate(cigarettes_source_ask_adult = case_when(
    (
      cigarettes_source_ask_adult_known == "Yes" |
        cigarettes_source_ask_adult_unknown == "Yes"
    ) ~ "Yes",
    (
      is.na(cigarettes_source_ask_adult_known) &
        is.na(cigarettes_source_ask_adult_unknown)
    ) ~ "Not applicable",
    TRUE ~ "No"
  )) %>%
  mutate(cigarettes_source_all_other = case_when(
    (
      cigarettes_source_siblings_provide == "Yes" |
        cigarettes_source_parents_provide == "Yes" |
        cigarettes_source_take_without_asking == "Yes" |
        cigarettes_source_other == "Yes" |
        cigarettes_source_friends_provide == "Yes" |
        cigarettes_source_ask_minor_known == "Yes"
    ) ~ "Yes",
    (
      is.na(cigarettes_source_siblings_provide) &
        is.na(cigarettes_source_parents_provide) &
        is.na(cigarettes_source_take_without_asking) &
        is.na(cigarettes_source_other) &
        is.na(cigarettes_source_friends_provide) &
        is.na(cigarettes_source_ask_minor_known)
    ) ~ "Not applicable",
    TRUE ~ "No"
  ))


# Frequency of using e-cigarettes / vaping at present (aggregated measure)
hwb_analysis$frequency_e_cigarettes_agg <-
  ifelse(
    hwb_analysis$frequency_e_cigarettes %in% c("Regular vapers", "Occasional vapers"),
    "Use of e-cigarettes at present",
    hwb_analysis$frequency_e_cigarettes
  )


# Use of e-cigarettes (aggregated measure)
su_analysis <- su_analysis %>%
  mutate(
    use_of_e_cigarettes = case_when(
      (
        e_cigarettes_use_frequency == "I use e-cigarettes / vapes sometimes, but no more than once a month" |
          e_cigarettes_use_frequency == "I have tried e-cigarettes / vapes a few times" |
          e_cigarettes_use_frequency == "I have tried an e-cigarette / vape once" |
          e_cigarettes_use_frequency == "I used to use e-cigarettes / vapes but don't use them anymore"
      ) ~ "I have tried e-cigarettes once or more.",
      e_cigarettes_use_frequency == "I use e-cigarettes / vapes once a week or more" ~ "I use e-cigarettes at least once a week (regular vaper).",
      (e_cigarettes_use_frequency == "I have never used an e-cigarette / vape" |
         e_cigarettes_use_frequency == "Prefer not to say") ~ "No"
    )
  )


# Smoking tobacco and use of e-cigarettes (derived measure)
su_analysis <- su_analysis %>%
  mutate(
    smoking_tobacco_and_e_cigarettes = case_when(
      (
        cigarettes_smoking_status == "Non-smokers" &
          use_of_e_cigarettes == "I have tried e-cigarettes once or more."
      ) ~ "I am a non-smoker of tobacco but have used e-cigarettes at least once.",
      (
        cigarettes_smoking_status == "Non-smokers" &
          use_of_e_cigarettes == "I use e-cigarettes at least once a week (regular vaper)."
      ) ~ "I am a non-smoker of tobacco but use e-cigarettes at least once a week (regular vaper).",
      (
        cigarettes_smoking_status == "Non-smokers" &
          use_of_e_cigarettes == "No"
      ) ~ "No" ### CHECK base cohort for this question
    )
  )


# How do you usually get your e-cigarettes/vapes/refills? (derived measure)
# As this is a tick-box question with participants allowed to select multiple answers, we create multiple columns
su_analysis <- su_analysis %>%
  mutate(e_cigarettes_source_shops = case_when(
    (
      e_cigarettes_source_supermarket == "Yes" |
        e_cigarettes_source_newsagent == "Yes" |
        e_cigarettes_source_garage_shop == "Yes" |
        e_cigarettes_source_van == "Yes" |
        e_cigarettes_source_hop_other == "Yes"
    )  ~ "Yes",
    (
      is.na(e_cigarettes_source_supermarket) &
        is.na(e_cigarettes_source_newsagent) &
        is.na(e_cigarettes_source_garage_shop) &
        is.na(e_cigarettes_source_van) &
        is.na(e_cigarettes_source_hop_other)
    )  ~ "Not applicable",
    TRUE ~ "No"
  )) %>%
  mutate(e_cigarettes_source_friends_relatives_someone_else = case_when(
    (
      e_cigarettes_source_friends_relatives == "Yes" |
        e_cigarettes_source_someone_else == "Yes"
    ) ~ "Yes",
    (
      is.na(e_cigarettes_source_friends_relatives) &
        is.na(e_cigarettes_source_someone_else)
    ) ~ "Not applicable",
    TRUE ~ "No"
  )) %>%
  mutate(e_cigarettes_source_ask_adult = case_when(
    (
      e_cigarettes_source_ask_adult_known == "Yes" |
        e_cigarettes_source_ask_adult_unknown == "Yes"
    ) ~ "Yes",
    (
      is.na(e_cigarettes_source_ask_adult_known) &
        is.na(e_cigarettes_source_ask_adult_unknown)
    ) ~ "Not applicable",
    TRUE ~ "No"
  )) %>%
  mutate(e_cigarettes_source_all_other = case_when(
    (
      e_cigarettes_source_siblings_provide == "Yes" |
        e_cigarettes_source_parents_provide == "Yes" |
        e_cigarettes_source_take_without_asking == "Yes" |
        e_cigarettes_source_other == "Yes" |
        e_cigarettes_source_friends_provide == "Yes" |
        e_cigarettes_source_ask_minor_known == "Yes"
    ) ~ "Yes",
    (
      is.na(e_cigarettes_source_siblings_provide) &
        is.na(e_cigarettes_source_parents_provide) &
        is.na(e_cigarettes_source_take_without_asking) &
        is.na(e_cigarettes_source_other) &
        is.na(e_cigarettes_source_friends_provide) &
        is.na(e_cigarettes_source_ask_minor_known)
    ) ~ "Not applicable",
    TRUE ~ "No"
  ))
  

# How often do you use drugs? (aggregated measure)
su_analysis <- su_analysis %>%
  mutate(
    drugs_use_frequency_agg = case_when(
      (
        drugs_use_frequency == "I take drugs at least once a week or more" |
          drugs_use_frequency == "I take drugs once or twice a month"
      ) ~ "I take drugs once or twice a month or I take drugs at least once a week or more",
      (drugs_use_frequency == "I used to take drugs sometimes but I don't take them anymore" | 
         drugs_use_frequency == "I have only taken drugs once" |
         drugs_use_frequency == "Prefer not to say" |
         drugs_use_frequency == "I take drugs a few times a year") ~ "No"
    ))

# Have you spent your own money on gambling activities in the last month? (derived measure)
hwb_analysis <- hwb_analysis %>%
  mutate(
    ever_gambled = case_when(
      gambling_none_of_above == "Yes" &
        gambling_lottery == "No" &
        gambling_betting == "No" &
        gambling_online == "No" &
        gambling_fruit_machines == "No" &
        gambling_betting_private == "No" &
        gambling_bingo == "No" &
        gambling_betting_shop == "No" &
        gambling_casino == "No" &
        gambling_other == "No" ~ "Never spent their own money on any gambling activities",
      
      (
        gambling_lottery == "Yes" |
          gambling_betting == "Yes" |
          gambling_online == "Yes" |
          gambling_fruit_machines == "Yes" |
          gambling_betting_private == "Yes" |
          gambling_bingo == "Yes" |
          gambling_betting_shop == "Yes" |
          gambling_casino == "Yes" |
          gambling_other == "Yes"
      ) ~ "Spent their own money on any gambling activities"
    )
  )



### 5 - Define variables for analysis ----

variables_hwb <- data.frame(
  variable = c("frequency_smoking", 
               "frequency_e_cigarettes", 
               "frequency_e_cigarettes_agg",
               "frequency_alcohol",
               "ever_gambled"),
  cat_order = c("cat_order_1", 
                "cat_order_2", 
                "cat_order_3",
                "cat_order_6",
                "cat_order_11")
)

variables_su <- data.frame(
  variable = c("cigarettes_smoking_status", 
               "cigarettes_source_shops",
               "cigarettes_source_street_market",
               "cigarettes_source_internet",
               "cigarettes_source_friends_relatives_someone_else",
               "cigarettes_source_ask_adult",
               "cigarettes_source_all_other",
               "e_cigarettes_use_frequency",
               "use_of_e_cigarettes",
               "smoking_tobacco_and_e_cigarettes",
               "e_cigarettes_source_shops",
               "e_cigarettes_source_street_market",
               "e_cigarettes_source_internet",
               "e_cigarettes_source_friends_relatives_someone_else",
               "e_cigarettes_source_ask_adult",
               "e_cigarettes_source_all_other",
               "alcohol_frequency_type_beer",
               "alcohol_frequency_type_wine",
               "alcohol_frequency_type_alcopops",
               "alcohol_frequency_type_spirits",
               "alcohol_frequency_type_cider",
               "alcohol_frequency_type_wine_fortified",
               "alcohol_frequency_type_other",
               "alcohol_ever_had_any",
               "alcohol_usual_source",
               "drugs_ever_taken",
               "drugs_use_frequency",
               "drugs_use_frequency_agg",
               "drugs_use_last_year_type_cannabis",
               "drugs_use_last_year_type_solvents",
               "drugs_use_last_year_type_amphetamines",
               "drugs_use_last_year_type_ecstasy",
               "drugs_use_last_year_type_benzos",
               "drugs_use_last_year_type_heroin",
               "drugs_use_last_year_type_mushrooms",
               "drugs_use_last_year_type_methadone",
               "drugs_use_last_year_type_mdma",
               "drugs_use_last_year_type_cocaine",
               "drugs_use_last_year_type_steroids",
               "drugs_use_last_year_type_gear",
               "drugs_use_last_year_type_ketamine",
               "drugs_use_last_year_type_synthetic_cannabinoids",
               "drugs_use_last_year_type_lsd",
               "drugs_use_last_year_type_2c",
               "drugs_use_last_year_type_diet_pills",
               "drugs_use_last_year_type_tanning_pills",
               "drugs_use_last_year_type_other"
               ),
  cat_order = c("cat_order_1", 
                "cat_order_9", 
                "cat_order_9",
                "cat_order_9",
                "cat_order_9",
                "cat_order_9",
                "cat_order_9",
                "cat_order_4",
                "cat_order_5",
                "cat_order_6",
                "cat_order_9", 
                "cat_order_9",
                "cat_order_9",
                "cat_order_9",
                "cat_order_9",
                "cat_order_9",
                "cat_order_9",
                "cat_order_9", 
                "cat_order_9",
                "cat_order_9",
                "cat_order_9",
                "cat_order_9",
                "cat_order_9",
                "cat_order_7",
                "cat_order_8",
                "cat_order_9",
                "cat_order_10",
                "cat_order_10",
                "cat_order_9", 
                "cat_order_9",
                "cat_order_9",
                "cat_order_9",
                "cat_order_9",
                "cat_order_9",
                "cat_order_9", 
                "cat_order_9",
                "cat_order_9",
                "cat_order_9",
                "cat_order_9",
                "cat_order_9",
                "cat_order_9", 
                "cat_order_9",
                "cat_order_9",
                "cat_order_9",
                "cat_order_9",
                "cat_order_9",
                "cat_order_9")
)



### 6 - Perform analysis on selected variables ----

# For national HWB
national_substance_use_hwb <- perform_analysis_national(hwb_analysis, variables_hwb)

# For each local authority HWB
local_authority_list_hwb <- list()

for (value in all_las) {
  cat("Processing local authority:", value, "\n")
  filtered_data <- hwb_analysis[hwb_analysis$pc_la == value, ]
  result <- perform_analysis_local_authority(filtered_data, variables_hwb)
  # Store the result with a dynamic name, e.g. Angus_substance_use
  list_name <- paste0(value, "_substance_use")
  local_authority_list_hwb[[list_name]] <- result
}



# For national substance use
national_substance_use_su <- perform_analysis_national(su_analysis, variables_su)

# For each local authority substance use
local_authority_list_su <- list()

for (value in all_las) {
  cat("Processing local authority:", value, "\n")
  filtered_data <- su_analysis[su_analysis$hwb_la == value, ]
  result <- perform_analysis_local_authority(filtered_data, variables_su)
  # Store the result with a dynamic name, e.g. Angus_substance_use
  list_name <- paste0(value, "_substance_use")
  local_authority_list_su[[list_name]] <- result
}


# Add national_substance_use_hwb to local_authority_list_hwb and store as all_hwb
all_hwb <- c(local_authority_list_hwb, list(National_substance_use = national_substance_use_hwb))

# Add national_substance_use_su to local_authority_list_su and store as all_su
all_su <- c(local_authority_list_su, list(National_substance_use = national_substance_use_su))



### 7 - Filter out rows we don't want and format derived questions ----

# The question "Of those who have ever had a proper alcoholic drink, which type of alcohol do you drink daily or weekly?" is odd because the base cohort is respondents
# who answered "Yes" to "Have you ever had a proper alcoholic drink?", so we need to convert this row to a number, and in the next step we will remove all other response
# options and the "Total" row and rename the row "Yes" to the "Total" row

# Iterate over each list within the list of tibbles
for (j in seq_along(all_su)) {
  # Iterate over each tibble within the inner list
  for (i in seq_along(all_su[[j]])) {
    # Subset the tibble where "Survey question" is "alcohol_ever_had_any" and "Response" is "Yes"
    subset_yes <- all_su[[j]][[i]][all_su[[j]][[i]]$`Survey question` == "alcohol_ever_had_any" & all_su[[j]][[i]]$Response == "Yes", ]
    
    # Subset the tibble where "Survey question" is "alcohol_ever_had_any" and "Response" is "Total"
    subset_total <- all_su[[j]][[i]][all_su[[j]][[i]]$`Survey question` == "alcohol_ever_had_any" & all_su[[j]][[i]]$Response == "Total", ]
    
    # Convert character cells to numeric temporarily for subset_yes and subset_total
    subset_yes[, 3:ncol(all_su[[j]][[i]])] <- lapply(subset_yes[, 3:ncol(all_su[[j]][[i]])], function(x) as.numeric(x))
    subset_total[, 3:ncol(all_su[[j]][[i]])] <- lapply(subset_total[, 3:ncol(all_su[[j]][[i]])], function(x) as.numeric(x))
    
    # Divide each number in the rows of subset_yes by 100
    subset_yes[, 3:ncol(all_su[[j]][[i]])] <- subset_yes[, 3:ncol(all_su[[j]][[i]])] / 100
    
    # Multiply each number in subset_yes by the corresponding number in subset_total
    subset_yes[, 3:ncol(all_su[[j]][[i]])] <- round(subset_yes[, 3:ncol(all_su[[j]][[i]])] * subset_total[, 3:ncol(all_su[[j]][[i]])], digits = 0)
    
    # Convert numeric cells back to character
    subset_yes[, 3:ncol(all_su[[j]][[i]])] <- lapply(subset_yes[, 3:ncol(all_su[[j]][[i]])], function(x) as.character(x))
    
    # Update the original tibble with the modified values
    all_su[[j]][[i]][all_su[[j]][[i]]$`Survey question` == "alcohol_ever_had_any" & all_su[[j]][[i]]$Response == "Yes", ] <- subset_yes
  }
}



# Questions which were tick boxes in SmartSurvey need to be reformatted from multiple questions to one
# Define a function to filter rows based on specified conditions
filter_rows <- function(tibble) {
  tibble %>%
    filter(!(`Survey question` == 'cigarettes_source_shops' & `Response` == 'No')) %>%
    filter(!(`Survey question` == 'cigarettes_source_shops' & `Response` == 'Total')) %>%
    filter(!(`Survey question` == 'cigarettes_source_street_market' & `Response` == 'No')) %>%
    filter(!(`Survey question` == 'cigarettes_source_street_market' & `Response` == 'Total')) %>%
    filter(!(`Survey question` == 'cigarettes_source_internet' & `Response` == 'No')) %>%
    filter(!(`Survey question` == 'cigarettes_source_internet' & `Response` == 'Total')) %>%
    filter(!(`Survey question` == 'cigarettes_source_friends_relatives_someone_else' & `Response` == 'No')) %>%
    filter(!(`Survey question` == 'cigarettes_source_friends_relatives_someone_else' & `Response` == 'Total')) %>%
    filter(!(`Survey question` == 'cigarettes_source_ask_adult' & `Response` == 'No')) %>%
    filter(!(`Survey question` == 'cigarettes_source_ask_adult' & `Response` == 'Total')) %>%
    filter(!(`Survey question` == 'cigarettes_source_all_other' & `Response` == 'No')) %>%
  
    filter(!(`Survey question` == 'e_cigarettes_source_shops' & `Response` == 'No')) %>%
    filter(!(`Survey question` == 'e_cigarettes_source_shops' & `Response` == 'Total')) %>%
    filter(!(`Survey question` == 'e_cigarettes_source_street_market' & `Response` == 'No')) %>%
    filter(!(`Survey question` == 'e_cigarettes_source_street_market' & `Response` == 'Total')) %>%
    filter(!(`Survey question` == 'e_cigarettes_source_internet' & `Response` == 'No')) %>%
    filter(!(`Survey question` == 'e_cigarettes_source_internet' & `Response` == 'Total')) %>%
    filter(!(`Survey question` == 'e_cigarettes_source_friends_relatives_someone_else' & `Response` == 'No')) %>%
    filter(!(`Survey question` == 'e_cigarettes_source_friends_relatives_someone_else' & `Response` == 'Total')) %>%
    filter(!(`Survey question` == 'e_cigarettes_source_ask_adult' & `Response` == 'No')) %>%
    filter(!(`Survey question` == 'e_cigarettes_source_ask_adult' & `Response` == 'Total')) %>%
    filter(!(`Survey question` == 'e_cigarettes_source_all_other' & `Response` == 'No')) %>%
    
    filter(!(`Survey question` == 'use_of_e_cigarettes' & `Response` == 'No')) %>%
    
    filter(!(`Survey question` == 'smoking_tobacco_and_e_cigarettes' & `Response` == 'No')) %>%
    
    filter(!(`Survey question` == 'drugs_use_frequency_agg' & `Response` == 'No')) %>%
  
    filter(!(`Survey question` == 'drugs_use_last_year_type_cannabis' & `Response` == 'No')) %>%
    filter(!(`Survey question` == 'drugs_use_last_year_type_cannabis' & `Response` == 'Total')) %>%
    filter(!(`Survey question` == 'drugs_use_last_year_type_solvents' & `Response` == 'No')) %>%
    filter(!(`Survey question` == 'drugs_use_last_year_type_solvents' & `Response` == 'Total')) %>%
    filter(!(`Survey question` == 'drugs_use_last_year_type_amphetamines' & `Response` == 'No')) %>%
    filter(!(`Survey question` == 'drugs_use_last_year_type_amphetamines' & `Response` == 'Total')) %>%
    filter(!(`Survey question` == 'drugs_use_last_year_type_ecstasy' & `Response` == 'No')) %>%
    filter(!(`Survey question` == 'drugs_use_last_year_type_ecstasy' & `Response` == 'Total')) %>%
    filter(!(`Survey question` == 'drugs_use_last_year_type_benzos' & `Response` == 'No')) %>%
    filter(!(`Survey question` == 'drugs_use_last_year_type_benzos' & `Response` == 'Total')) %>%
    filter(!(`Survey question` == 'drugs_use_last_year_type_heroin' & `Response` == 'No')) %>%
    filter(!(`Survey question` == 'drugs_use_last_year_type_heroin' & `Response` == 'Total')) %>%
    filter(!(`Survey question` == 'drugs_use_last_year_type_mushrooms' & `Response` == 'No')) %>%
    filter(!(`Survey question` == 'drugs_use_last_year_type_mushrooms' & `Response` == 'Total')) %>%
    filter(!(`Survey question` == 'drugs_use_last_year_type_methadone' & `Response` == 'No')) %>%
    filter(!(`Survey question` == 'drugs_use_last_year_type_methadone' & `Response` == 'Total')) %>%
    filter(!(`Survey question` == 'drugs_use_last_year_type_mdma' & `Response` == 'No')) %>%
    filter(!(`Survey question` == 'drugs_use_last_year_type_mdma' & `Response` == 'Total')) %>%
    filter(!(`Survey question` == 'drugs_use_last_year_type_cocaine' & `Response` == 'No')) %>%
    filter(!(`Survey question` == 'drugs_use_last_year_type_cocaine' & `Response` == 'Total')) %>%
    filter(!(`Survey question` == 'drugs_use_last_year_type_steroids' & `Response` == 'No')) %>%
    filter(!(`Survey question` == 'drugs_use_last_year_type_steroids' & `Response` == 'Total')) %>%
    filter(!(`Survey question` == 'drugs_use_last_year_type_gear' & `Response` == 'No')) %>%
    filter(!(`Survey question` == 'drugs_use_last_year_type_gear' & `Response` == 'Total')) %>%
    filter(!(`Survey question` == 'drugs_use_last_year_type_ketamine' & `Response` == 'No')) %>%
    filter(!(`Survey question` == 'drugs_use_last_year_type_ketamine' & `Response` == 'Total')) %>%
    filter(!(`Survey question` == 'drugs_use_last_year_type_synthetic_cannabinoids' & `Response` == 'No')) %>%
    filter(!(`Survey question` == 'drugs_use_last_year_type_synthetic_cannabinoids' & `Response` == 'Total')) %>%
    filter(!(`Survey question` == 'drugs_use_last_year_type_lsd' & `Response` == 'No')) %>%
    filter(!(`Survey question` == 'drugs_use_last_year_type_lsd' & `Response` == 'Total')) %>%
    filter(!(`Survey question` == 'drugs_use_last_year_type_2c' & `Response` == 'No')) %>%
    filter(!(`Survey question` == 'drugs_use_last_year_type_2c' & `Response` == 'Total')) %>%
    filter(!(`Survey question` == 'drugs_use_last_year_type_diet_pills' & `Response` == 'No')) %>%
    filter(!(`Survey question` == 'drugs_use_last_year_type_diet_pills' & `Response` == 'Total')) %>%
    filter(!(`Survey question` == 'drugs_use_last_year_type_tanning_pills' & `Response` == 'No')) %>%
    filter(!(`Survey question` == 'drugs_use_last_year_type_tanning_pills' & `Response` == 'Total')) %>%
    filter(!(`Survey question` == 'drugs_use_last_year_type_other' & `Response` == 'No')) %>%
    
    filter(!(`Survey question` == 'alcohol_frequency_type_beer' & `Response` == 'Every month')) %>%
    filter(!(`Survey question` == 'alcohol_frequency_type_beer' & `Response` == 'Never')) %>%
    filter(!(`Survey question` == 'alcohol_frequency_type_beer' & `Response` == 'Rarely')) %>%
    filter(!(`Survey question` == 'alcohol_frequency_type_beer' & `Response` == 'Total')) %>%
    filter(!(`Survey question` == 'alcohol_frequency_type_wine' & `Response` == 'Every month')) %>%
    filter(!(`Survey question` == 'alcohol_frequency_type_wine' & `Response` == 'Never')) %>%
    filter(!(`Survey question` == 'alcohol_frequency_type_wine' & `Response` == 'Rarely')) %>%
    filter(!(`Survey question` == 'alcohol_frequency_type_wine' & `Response` == 'Total')) %>%
    filter(!(`Survey question` == 'alcohol_frequency_type_alcopops' & `Response` == 'Every month')) %>%
    filter(!(`Survey question` == 'alcohol_frequency_type_alcopops' & `Response` == 'Never')) %>%
    filter(!(`Survey question` == 'alcohol_frequency_type_alcopops' & `Response` == 'Rarely')) %>%
    filter(!(`Survey question` == 'alcohol_frequency_type_alcopops' & `Response` == 'Total')) %>%
    filter(!(`Survey question` == 'alcohol_frequency_type_spirits' & `Response` == 'Every month')) %>%
    filter(!(`Survey question` == 'alcohol_frequency_type_spirits' & `Response` == 'Never')) %>%
    filter(!(`Survey question` == 'alcohol_frequency_type_spirits' & `Response` == 'Rarely')) %>%
    filter(!(`Survey question` == 'alcohol_frequency_type_spirits' & `Response` == 'Total')) %>%
    filter(!(`Survey question` == 'alcohol_frequency_type_cider' & `Response` == 'Every month')) %>%
    filter(!(`Survey question` == 'alcohol_frequency_type_cider' & `Response` == 'Never')) %>%
    filter(!(`Survey question` == 'alcohol_frequency_type_cider' & `Response` == 'Rarely')) %>%
    filter(!(`Survey question` == 'alcohol_frequency_type_cider' & `Response` == 'Total')) %>%
    filter(!(`Survey question` == 'alcohol_frequency_type_wine_fortified' & `Response` == 'Every month')) %>%
    filter(!(`Survey question` == 'alcohol_frequency_type_wine_fortified' & `Response` == 'Never')) %>%
    filter(!(`Survey question` == 'alcohol_frequency_type_wine_fortified' & `Response` == 'Rarely')) %>%
    filter(!(`Survey question` == 'alcohol_frequency_type_wine_fortified' & `Response` == 'Total')) %>%
    filter(!(`Survey question` == 'alcohol_frequency_type_other' & `Response` == 'Every month')) %>%
    filter(!(`Survey question` == 'alcohol_frequency_type_other' & `Response` == 'Never')) %>%
    filter(!(`Survey question` == 'alcohol_frequency_type_other' & `Response` == 'Rarely')) %>%
    filter(!(`Survey question` == 'alcohol_frequency_type_other' & `Response` == 'Total')) %>%
    filter(!(`Survey question` == 'alcohol_ever_had_any' & `Response` == 'No')) %>%
    filter(!(`Survey question` == 'alcohol_ever_had_any' & `Response` == 'Prefer not to say')) %>%
    filter(!(`Survey question` == 'alcohol_ever_had_any' & `Response` == 'Total'))
}

# Apply the function to each tibble in the lists all_hwb and all_su
all_hwb <- lapply(all_hwb, function(sublist) {
  map(sublist, filter_rows)
})

all_su <- lapply(all_su, function(sublist) {
  map(sublist, filter_rows)
})


# Function to replace values in 'Response' column
replace_values <- function(tibble) {
  tibble$Response[tibble$'Survey question' == 'cigarettes_source_shops' & tibble$Response == 'Yes'] <- 'I buy them from a supermarket, newsagent, tobacconist, sweet shop, garage shop, van, such as an ice cream van or burger van or some other type of shop.'
  tibble$Response[tibble$'Survey question' == 'cigarettes_source_street_market' & tibble$Response == 'Yes'] <- 'I buy them from a street market.'
  tibble$Response[tibble$'Survey question' == 'cigarettes_source_internet' & tibble$Response == 'Yes'] <- 'I buy them on the internet.'
  tibble$Response[tibble$'Survey question' == 'cigarettes_source_friends_relatives_someone_else' & tibble$Response == 'Yes'] <- 'I buy cigarettes/tobacco from friends, relatives or someone else.'
  tibble$Response[tibble$'Survey question' == 'cigarettes_source_ask_adult' & tibble$Response == 'Yes'] <- 'I ask an adult I know or don’t know to buy me cigarettes/tobacco.'
  tibble$Response[tibble$'Survey question' == 'cigarettes_source_all_other' & tibble$Response == 'Yes'] <- 'My mother, father, carer, brother, sister, friends gives me cigarettes/tobacco or I take cigarettes/tobacco without asking or I get cigarettes/tobacco in some other way.'
  
  tibble$Response[tibble$'Survey question' == 'e_cigarettes_source_shops' & tibble$Response == 'Yes'] <- 'I buy them from a supermarket, newsagent, tobacconist, sweet shop, garage shop, van, such as an ice cream van or burger van or some other type of shop.'
  tibble$Response[tibble$'Survey question' == 'e_cigarettes_source_street_market' & tibble$Response == 'Yes'] <- 'I buy them from a street market.'
  tibble$Response[tibble$'Survey question' == 'e_cigarettes_source_internet' & tibble$Response == 'Yes'] <- 'I buy them on the internet.'
  tibble$Response[tibble$'Survey question' == 'e_cigarettes_source_friends_relatives_someone_else' & tibble$Response == 'Yes'] <- 'I buy e-cigarettes/refills from friends, relatives or someone else.'
  tibble$Response[tibble$'Survey question' == 'e_cigarettes_source_ask_adult' & tibble$Response == 'Yes'] <- 'I ask an adult I know or don’t know to buy me e-cigarettes/refills.'
  tibble$Response[tibble$'Survey question' == 'e_cigarettes_source_all_other' & tibble$Response == 'Yes'] <- 'My mother, father, carer, brother, sister, friends gives me e-cigarettes/refills or I take e-cigarettes/refills without asking or I get e-cigarettes/refills in some other way.'
  
  tibble$Response[tibble$'Survey question' == 'drugs_use_last_year_type_cannabis' & tibble$Response == 'Yes'] <- 'Cannabis (Weed, Skunk, Green, Hash, Blow, Joints, Marijuana)'
  tibble$Response[tibble$'Survey question' == 'drugs_use_last_year_type_solvents' & tibble$Response == 'Yes'] <- 'Gas, Glue or Other Solvents'
  tibble$Response[tibble$'Survey question' == 'drugs_use_last_year_type_amphetamines' & tibble$Response == 'Yes'] <- 'Amphetamines (Speed, Whizz, Sulph, Paste)'
  tibble$Response[tibble$'Survey question' == 'drugs_use_last_year_type_ecstasy' & tibble$Response == 'Yes'] <- 'Ecstasy (E, Eccies, XTC, Pills)'
  tibble$Response[tibble$'Survey question' == 'drugs_use_last_year_type_benzos' & tibble$Response == 'Yes'] <- 'Benzos (Valium, Vallies, Blues, Whites, Yellows, Xanax)'
  tibble$Response[tibble$'Survey question' == 'drugs_use_last_year_type_heroin' & tibble$Response == 'Yes'] <- 'Heroin (Smack, Kit, H, Brown, Skag)'
  tibble$Response[tibble$'Survey question' == 'drugs_use_last_year_type_mushrooms' & tibble$Response == 'Yes'] <- 'Magic Mushrooms (Shrooms)'
  tibble$Response[tibble$'Survey question' == 'drugs_use_last_year_type_methadone' & tibble$Response == 'Yes'] <- 'Methadone (Physeptone, Meth)'
  tibble$Response[tibble$'Survey question' == 'drugs_use_last_year_type_mdma' & tibble$Response == 'Yes'] <- 'MDMA crystals/crystals (Mandy, Molly, Madman)'
  tibble$Response[tibble$'Survey question' == 'drugs_use_last_year_type_cocaine' & tibble$Response == 'Yes'] <- 'Cocaine (Coke, Charlie, C, Proper, Council)'
  tibble$Response[tibble$'Survey question' == 'drugs_use_last_year_type_steroids' & tibble$Response == 'Yes'] <- 'Anabolic Steroids (Roids)'
  tibble$Response[tibble$'Survey question' == 'drugs_use_last_year_type_gear' & tibble$Response == 'Yes'] <- 'Unknown White Powders (Gear)'
  tibble$Response[tibble$'Survey question' == 'drugs_use_last_year_type_ketamine' & tibble$Response == 'Yes'] <- 'Ketamine (Ket, K)'
  tibble$Response[tibble$'Survey question' == 'drugs_use_last_year_type_synthetic_cannabinoids' & tibble$Response == 'Yes'] <- 'Synthetic Cannabinoids (SPICE, RedExodus, Mamba)'
  tibble$Response[tibble$'Survey question' == 'drugs_use_last_year_type_lsd' & tibble$Response == 'Yes'] <- 'LSD (Acid, Blotters)'
  tibble$Response[tibble$'Survey question' == 'drugs_use_last_year_type_2c' & tibble$Response == 'Yes'] <- '2C (2CB, 2CI, 2CE)'
  tibble$Response[tibble$'Survey question' == 'drugs_use_last_year_type_diet_pills' & tibble$Response == 'Yes'] <- 'Diet Pills'
  tibble$Response[tibble$'Survey question' == 'drugs_use_last_year_type_tanning_pills' & tibble$Response == 'Yes'] <- 'Tanning Pills'
  tibble$Response[tibble$'Survey question' == 'drugs_use_last_year_type_other' & tibble$Response == 'Yes'] <- 'Other drugs including prescription drugs not prescribed to you'
  
  tibble$Response[tibble$'Survey question' == 'alcohol_ever_had_any' & tibble$Response == 'Yes'] <- 'Total'
  return(tibble)
}

# Apply the function to each tibble in all_hwb and all_su
all_hwb <- map(all_hwb, function(inner_list) {
  map(inner_list, replace_values)
})

all_su <- map(all_su, function(inner_list) {
  map(inner_list, replace_values)
})


# Function to replace values in 'Survey question' column
replace_survey_question <- function(tibble) {
  tibble <- tibble %>%
    mutate(`Survey question` = case_when(
      `Survey question` %in% c("cigarettes_source_shops", "cigarettes_source_street_market", "cigarettes_source_internet",
                               "cigarettes_source_friends_relatives_someone_else", "cigarettes_source_ask_adult", "cigarettes_source_all_other") ~ "How do you usually get your cigarettes/tobacco?",
      
      `Survey question` %in% c("e_cigarettes_source_shops", "e_cigarettes_source_street_market", "e_cigarettes_source_internet",
                               "e_cigarettes_source_friends_relatives_someone_else", "e_cigarettes_source_ask_adult", "e_cigarettes_source_all_other") ~ "How do you usually get your e-cigarettes/vapes/refills?",
      
      `Survey question` %in% c("drugs_use_last_year_type_cannabis", "drugs_use_last_year_type_solvents", "drugs_use_last_year_type_amphetamines",
                               "drugs_use_last_year_type_ecstasy", "drugs_use_last_year_type_benzos", "drugs_use_last_year_type_heroin",
                               "drugs_use_last_year_type_mushrooms", "drugs_use_last_year_type_methadone", "drugs_use_last_year_type_mdma",
                               "drugs_use_last_year_type_cocaine", "drugs_use_last_year_type_steroids", "drugs_use_last_year_type_gear",
                               "drugs_use_last_year_type_ketamine", "drugs_use_last_year_type_synthetic_cannabinoids", "drugs_use_last_year_type_lsd",
                               "drugs_use_last_year_type_2c", "drugs_use_last_year_type_diet_pills", "drugs_use_last_year_type_tanning_pills",
                               "drugs_use_last_year_type_other") ~ "Of those who have taken drugs ever, and taken drugs in the last month, which (if any) of these drugs have you taken in the last year?",
      
      `Survey question` %in% c("alcohol_frequency_type_beer", "alcohol_frequency_type_wine", "alcohol_frequency_type_alcopops",
                               "alcohol_frequency_type_spirits", "alcohol_frequency_type_cider", "alcohol_frequency_type_wine_fortified",
                               "alcohol_frequency_type_other", "alcohol_ever_had_any") ~ "Of those who have ever had a proper alcoholic drink, which type of alcohol do you drink daily or weekly?",
       TRUE ~ `Survey question`
    ))
  return(tibble)
}

# # Replace values in 'Survey question' column for each tibble in the list all_hwb and all_su
all_hwb <- map(all_hwb, function(inner_list) {
  map(inner_list, replace_survey_question)
})

all_su <- map(all_su, function(inner_list) {
  map(inner_list, replace_survey_question)
})



### 8 - Save outputs as an excel file to Merged folder ----

# Get names of all folders we want to save substance use data to
folder_names <- sub("_.*", "", names(all_hwb))

# Save local authorities
# Function to save tibbles in respective folders
save_tibbles_as_sheets_hwb <- function(tibble_list, folder_name) {
  file_path <- here::here("output", year, folder_name, paste0(year, "_substance_use_(S2_and_S4_stage_questionnaire).xlsx"))
  write_xlsx(
    tibble_list,
    path = file_path,
    col_names = TRUE
  )
}

# Save each list of tibbles as a single Excel file with multiple sheets
map2(all_hwb, folder_names, save_tibbles_as_sheets_hwb)

# Save local authorities
# Function to save tibbles in respective folders
save_tibbles_as_sheets_su <- function(tibble_list, folder_name) {
  file_path <- here::here("output", year, folder_name, paste0(year, "_substance_use_(S4_substance_use_survey).xlsx"))
  write_xlsx(
    tibble_list,
    path = file_path,
    col_names = TRUE
  )
}

# Save each list of tibbles as a single Excel file with multiple sheets
map2(all_su, folder_names, save_tibbles_as_sheets_su)



### END OF SCRIPT ###

