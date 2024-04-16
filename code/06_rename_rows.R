#########################################################################
# Name of file - 06_check_validate_rows.R
# Health and Wellbeing (HWB) Census 
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 4.2.2
#
# Description - Renames rows of submitted data, where row response options have been changed,
# but the original meaning is retained
#########################################################################


### 0 - Setup ----

source(here::here("code", "00_setup.R"))



### 1 - Read in unvalidated rows ----

# Read in merged data for each survey and store as a list of data frames
unvalidated_rows <- 
  map(
    survey_names,
    ~ read_xlsx(file.path(raw_data_folder, year, "Merged", "04_merged_data.xlsx"), sheet = .x)
  )



### 2 - Replace all "-"s with "NA"s ----

validated_rows <- lapply(unvalidated_rows, function(tibble) {
  tibble[tibble == "-"] <- NA
  return(tibble)
})



### 3 - Replace responses which occur in multiple questions for multiple stages ----

# Replace "Neither agree not disagree" with "Neither agree nor disagree" in every tibble
# Replace "Prefer Not to Say" with "Prefer not to say" in every tibble
# Replace "Don't Know" with "Don’t know" in every tibble (different apostrophe and capitalised "K")
# Replace "Don’t know" with "Don't know" in every tibble (different apostrophes)
# Replace "Doesn’t apply to me" with "Doesn't apply to me" in every tibble (different apostrophes)

replacements <- c(
  "Neither agree not disagree" = "Neither agree nor disagree",
  "Prefer Not to Say" = "Prefer not to say",
  "Don’t Know" = "Don't know",
  "Don’t know" = "Don't know",
  "Doesn’t apply to me" = "Doesn't apply to me"
)

validated_rows <- lapply(validated_rows, function(tibble) {
  tibble %>% mutate_all(recode, !!!replacements)
})



### 4 - For S3 the question frequency_physical_activity ----

# Replace "At least once a month but not every week" with "Once a month"
validated_rows$S3 <- validated_rows$S3 %>%
  mutate(frequency_physical_activity = 
           ifelse(frequency_physical_activity == "At least once a month but not every week", "Once a month", frequency_physical_activity))



### 5 - For P5, P6, P7 the question frequency_feeling_lonely ----

# Replace "Often" with "Often or always" for P5 & P6
validated_rows <- map_if(validated_rows, names(validated_rows) %in% c("P5", "P6"), ~ .x %>%
                           mutate(frequency_feeling_lonely = 
                                    ifelse(frequency_feeling_lonely == "Often", "Often or always", frequency_feeling_lonely)))

# Replace "Some of the time" with "Sometimes" for P5 & P6
validated_rows <- map_if(validated_rows, names(validated_rows) %in% c("P5", "P6"), ~ .x %>%
                           mutate(frequency_feeling_lonely = 
                                    ifelse(frequency_feeling_lonely == "Some of the time", "Sometimes", frequency_feeling_lonely)))

# Replace "Sometimes" with "Some of the time" for P7
validated_rows$P7 <- validated_rows$P7 %>%
  mutate(frequency_feeling_lonely = 
           ifelse(frequency_feeling_lonely == "Sometimes", "Some of the time", frequency_feeling_lonely))



### 6 - For S4-S6 replace "Doesn't apply to me" with NA for the following questions ----
# sex_boyfriend_girlfriend_feel_safe
# sex_boyfriend_girlfriend_encourages
# sex_boyfriend_girlfriend_checks
# sex_boyfriend_girlfriend_pressures_sexual
validated_rows <- 
  map_if(validated_rows, names(validated_rows) %in% c("S4", "S5", "S6"), ~ .x %>%
           mutate(across(
             c(sex_boyfriend_girlfriend_feel_safe, 
               sex_boyfriend_girlfriend_encourages, 
               sex_boyfriend_girlfriend_checks,
               sex_boyfriend_girlfriend_pressures_sexual),
             ~ ifelse(. == "Doesn't apply to me", NA, .)))
  )


### 7 - For S4-S6 replace responses to sex_experience_type ----

# Replace "Sexual intercourse" with "Vaginal or anal sex"
validated_rows <- map_if(validated_rows, names(validated_rows) %in% c("S4", "S5", "S6"), ~ .x %>%
                           mutate(sex_experience_type = 
                                    ifelse(sex_experience_type == "Sexual intercourse", "Vaginal or anal sex", sex_experience_type)))

# Replace "Penetrative sex" with "Vaginal or anal sex"
validated_rows <- map_if(validated_rows, names(validated_rows) %in% c("S4", "S5", "S6"), ~ .x %>%
                           mutate(sex_experience_type = 
                                    ifelse(sex_experience_type == "Penetrative sex", "Vaginal or anal sex", sex_experience_type)))

# Replace "More intimate experiences (not sexual intercourse)" with "Some experiences but no sexual intercourse (e.g. touching intimately underneath clothes or without clothes on)"
validated_rows <- map_if(validated_rows, names(validated_rows) %in% c("S4", "S6"), ~ .x %>%
                           mutate(sex_experience_type = 
                                    ifelse(sex_experience_type == "More intimate experiences (not sexual intercourse)", 
                                           "Some experiences but no sexual intercourse (e.g. touching intimately underneath clothes or without clothes on)", sex_experience_type)))

# Replace "More intimate experience (not sexual intercourse)" with "Some experiences but no sexual intercourse (e.g. touching intimately underneath clothes or without clothes on)"
validated_rows <- map_if(validated_rows, names(validated_rows) %in% c("S5"), ~ .x %>%
                           mutate(sex_experience_type = 
                                    ifelse(sex_experience_type == "More intimate experience (not sexual intercourse)", 
                                           "Some experiences but no sexual intercourse (e.g. touching intimately underneath clothes or without clothes on)", sex_experience_type)))



### 8 - Replace response options for column simd_decile for substance use ----

# Dundee, Renfrewshire, North Ayrshire, Stirling, Moray, Scottish Borders, Shetland, Edinburgh City collected vigintiles, instead of deciles, so we need to convert vigintiles 
# Dumfries & Galloway, South Ayrshire, Angus, Clackmannanshire and Glasgow collected deciles
# Falkirk, Perth & Kinross and East Renfrewshire did not collected SIMD
# However, analysis is published by SIMD quintile so we will convert all deciles and vigintiles to quintiles now

# Define the vigintile rules
vigintile_rules <- function(x) {
  ifelse(
    x %in% c("1", "2", "3", "4"), "1",
    ifelse(
      x %in% c("5", "6", "7", "8"), "2",
      ifelse(
        x %in% c("9", "10", "11", "12"), "3",
        ifelse(
          x %in% c("13", "14", "15", "16"), "4",
          ifelse(
            x %in% c("17", "18", "19", "20"), "5",
            NA
          )
        )
      )
    )
  )
}

# Apply the vigintile rules to the simd_decile column based on the LA
validated_rows$S4_SU <- validated_rows$S4_SU %>%
  mutate(
    simd_decile = ifelse(
      hwb_la %in% c("Dundee", "Renfrewshire", "North Ayrshire", "Stirling", "Moray", "Scottish Borders", "Shetland", "Edinburgh City"),
      vigintile_rules(simd_decile),
      simd_decile
    )
  )


# Define the decile rules
decile_rules <- function(x) {
  ifelse(
    x %in% c("1", "2"), "1",
    ifelse(
      x %in% c("3", "4"), "2",
      ifelse(
        x %in% c("5", "6"), "3",
        ifelse(
          x %in% c("7", "8"), "4",
          ifelse(
            x %in% c("9", "10"), "5",
            NA
          )
        )
      )
    )
  )
}

# Apply the decile rules to the simd_decile column based on the LA
validated_rows$S4_SU <- validated_rows$S4_SU %>%
  mutate(
    simd_decile = ifelse(
      hwb_la %in% c("Dumfries & Galloway", "South Ayrshire", "Angus", "Clackmannanshire", "Glasgow"),
      decile_rules(simd_decile),
      simd_decile
    )
  )



### 9 - Replace response options for column cigarettes_daily_number for substance use ----

validated_rows$S4_SU <- validated_rows$S4_SU %>%
  mutate(cigarettes_daily_number = case_when(
    cigarettes_daily_number == "01-Feb" ~ "1-2",
    cigarettes_daily_number == "03-Apr" ~ "3-4",
    cigarettes_daily_number == "05-Jun" ~ "5-6",
    cigarettes_daily_number == "07-Aug" ~ "7-8",
    cigarettes_daily_number == "09-Oct" ~ "9-10",
    cigarettes_daily_number == "44593" ~ "1-2",
    cigarettes_daily_number == "44654" ~ "3-4",
    cigarettes_daily_number == "44717" ~ "5-6",
    cigarettes_daily_number == "44780" ~ "7-8",
    cigarettes_daily_number == "44843" ~ "9-10",
    TRUE ~ cigarettes_daily_number
  ))



### 10 - Save as excel file to Merged folder ----

write_xlsx(
  validated_rows,
  file.path(raw_data_folder, year, "Merged", paste0("05_validated_rows.xlsx"))
)


### END OF SCRIPT ###
