#########################################################################
# Name of file - 06_check_validate_rows.R
# Health and Wellbeing (HWB) Census 
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 4.2.2
#
# Description - Renames rows of submitted datafiles, where row response options have been changed,
# but the original meaning is retained
#########################################################################


### 0 - Setup ----

source(here::here("code", "00_setup.R"))



### 1 - Read in unvalidated rows ----

# Read in merged data for each stage and store as a list of data frames
# Define the path to your Excel file and the sheet names
file_path <- file.path(raw_data_folder, year, "Merged", "04_merged_data.xlsx")

# Read in all sheets into a list of tibbles with names
unvalidated_rows <- set_names(
  map(
    all_stages,
    ~ read_xlsx(file_path, sheet = .x)
  ),
  all_stages
)



### 2 - Replace all "-"s with "NA"s ----

validated_rows <- lapply(unvalidated_rows, function(tibble) {
  tibble[tibble == "-"] <- NA
  return(tibble)
})



### 3 - Replace responses which occur in multiple questions for multiple stages ----

# Replace "Neither agree not disagree" with "Neither agree nor disagree" in every tibble
validated_rows <- lapply(validated_rows, function(tibble) {
  tibble[tibble == "Neither agree not disagree"] <- "Neither agree nor disagree"
  return(tibble)
})

# Replace "Prefer Not to Say" with "Prefer not to say" in every tibble
validated_rows <- lapply(validated_rows, function(tibble) {
  tibble[tibble == "Prefer Not to Say"] <- "Prefer not to say"
  return(tibble)
})

# Replace "Don't Know" with "Don't know" in every tibble
validated_rows <- lapply(validated_rows, function(tibble) {
  tibble[tibble == "Don't Know"] <- "Don't know"
  return(tibble)
})

# Replace "Don’t know" with "Don't know" in every tibble (different apostrophes)
validated_rows <- lapply(validated_rows, function(tibble) {
  tibble[tibble == "Don’t know"] <- "Don't know"
  return(tibble)
})

# Replace "Doesn’t apply to me" with "Doesn't apply to me" in every tibble (different apostrophes)
validated_rows <- lapply(validated_rows, function(tibble) {
  tibble[tibble == "Doesn’t apply to me"] <- "Doesn't apply to me"
  return(tibble)
})



### 4 - For P3 the question frequency_physical_activity ----

# Replace "At least once a month but not every week" with "Once a month"
validated_rows <- map_if(validated_rows, names(validated_rows) %in% c("S4", "S5", "S6"), ~ .x %>%
                           mutate(frequency_physical_activity = 
                                    ifelse(frequency_physical_activity == "At least once a month but not every week", "Once a month", frequency_physical_activity)))



### 5 - For P5 & P6 the question frequency_feeling_lonely ----

# Replace "Often" with "Often or always"
validated_rows <- map(validated_rows, ~ .x %>% 
                        mutate(frequency_feeling_lonely = 
                                 ifelse(frequency_feeling_lonely == "Often", "Often or always", frequency_feeling_lonely)))

# Replace "Some of the time" with "Sometimes"
validated_rows <- map(validated_rows, ~ .x %>% 
                        mutate(frequency_feeling_lonely = 
                                 ifelse(frequency_feeling_lonely == "Some of the time", "Sometimes", frequency_feeling_lonely)))



### 6 - For S4-S6 replace "Doesn't apply to me" with NA for the following questions ----
# sex_boyfriend_girlfriend_feel_safe
# sex_boyfriend_girlfriend_encourages
# sex_boyfriend_girlfriend_checks
# sex_boyfriend_girlfriend_pressures_sexual
validated_rows <- map_if(validated_rows, names(validated_rows) %in% c("S4", "S5", "S6"), ~ .x %>%
                           mutate(sex_boyfriend_girlfriend_feel_safe = 
                                    ifelse(sex_boyfriend_girlfriend_feel_safe == "Doesn't apply to me", NA, sex_boyfriend_girlfriend_feel_safe)))

validated_rows <- map_if(validated_rows, names(validated_rows) %in% c("S4", "S5", "S6"), ~ .x %>%
                           mutate(sex_boyfriend_girlfriend_encourages = 
                                    ifelse(sex_boyfriend_girlfriend_encourages == "Doesn't apply to me", NA, sex_boyfriend_girlfriend_encourages)))

validated_rows <- map_if(validated_rows, names(validated_rows) %in% c("S4", "S5", "S6"), ~ .x %>%
                           mutate(sex_boyfriend_girlfriend_checks = 
                                    ifelse(sex_boyfriend_girlfriend_checks == "Doesn't apply to me", NA, sex_boyfriend_girlfriend_checks)))

validated_rows <- map_if(validated_rows, names(validated_rows) %in% c("S4", "S5", "S6"), ~ .x %>%
                           mutate(sex_boyfriend_girlfriend_pressures_sexual = 
                                    ifelse(sex_boyfriend_girlfriend_pressures_sexual == "Doesn't apply to me", NA, sex_boyfriend_girlfriend_pressures_sexual)))



### 7 - For S4-S6 replace responses to sex_experience_type ----

# Replace "Sexual intercourse" with "Vaginal or anal sex"
validated_rows <- map_if(validated_rows, names(validated_rows) %in% c("S4", "S5", "S6"), ~ .x %>%
                           mutate(sex_experience_type = 
                                    ifelse(sex_experience_type == "Sexual intercourse", "Vaginal or anal sex", sex_experience_type)))

# Replace "Penetrative sex" with "Vaginal or anal sex"
validated_rows <- map_if(validated_rows, names(validated_rows) %in% c("S4", "S5", "S6"), ~ .x %>%
                           mutate(sex_experience_type = 
                                    ifelse(sex_experience_type == "Penetrative sex", "Vaginal or anal sex", sex_experience_type)))

# Replace "Penetrative sex" with "Some experiences but no sexual intercourse (e.g. touching intimately underneath clothes or without clothes on)"
validated_rows <- map_if(validated_rows, names(validated_rows) %in% c("S4", "S5", "S6"), ~ .x %>%
                           mutate(sex_experience_type = 
                                    ifelse(sex_experience_type == "More intimate experiences (not sexual intercourse)", 
                                           "Some experiences but no sexual intercourse (e.g. touching intimately underneath clothes or without clothes on)", sex_experience_type)))



### 8 - Save as excel file to Merged folder ----

write_xlsx(
  validated_rows,
  file.path(raw_data_folder, year, "Merged", paste0("05_validated_rows.xlsx"))
)


### END OF SCRIPT ###
