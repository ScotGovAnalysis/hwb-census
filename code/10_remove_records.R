#########################################################################
# Name of file - 10_remove_rows.R
# Health and Wellbeing (HWB) Census 
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 4.2.2
#
# Description - This code removes records for pupils who answered the 
# wrong stage questionnaire, or who responded more than once, or who cannot
# be linked to the pupil census for analysis. This script only looks at HWB 
# data, not substance use.
#########################################################################


### 0 - Setup ----

source(here::here("config.R"))
source(here::here("code", "00_setup.R"))
source(here::here("functions", "read_raw_data.R"))
source(here::here("lookups", "lookup_la_code.R"))



### 1 - Read in raw data ----

# Read all sheets into a list of tibbles (HWB data only)
raw_data <- set_names(
  map(
    all_stages,
    ~ read_xlsx(file.path(raw_data_folder, year, "Merged", "07_validated_school_names_hwb.xlsx"), sheet = .x)
  ),
  all_stages
)
  


### 2 - Add "hwb_stage" column ----

# Add "hwb_stage" column at the beginning of each tibble with respective stage (taken from tibble names)
modified_data <- map2(raw_data, names(raw_data), ~ .x %>% 
                        mutate(hwb_stage = .y) %>% 
                        relocate(hwb_stage, .before = 1))

# Rename column "scn" to "hwb_scn" for ease of comparison to the pupil census
modified_data <- lapply(modified_data, function(tibble) {
  tibble <- dplyr::rename(tibble, hwb_scn = scn)
})



### 3 - Join all stages into one dataframe ----

# Joins all stages into one dataframe, whilst maintaining a logical ordering of the order in which questions were asked.
# The ordering is determined by all cols in the first tibble (in order), then all cols in the second tibble that aren't in the first tibble (in
# order), then all cols in the third tibble that aren't in the first two tibbles (in order) etc.

# Function to fill missing columns with "Question not asked of stage"
fill_missing_cols <- function(df, all_cols) {
  missing_cols <- setdiff(all_cols, colnames(df))
  if (length(missing_cols) > 0) {
    df[missing_cols] <- "Question not asked of stage"
  }
  return(df)
}

# Get all unique column names across all tibbles
all_cols <- unique(unlist(lapply(modified_data, names)))

# Initialize an empty dataframe
all_stages_dataframe <- tibble()

# Iterate through modified_data to create the final dataframe
for (tbl in modified_data) {
  tbl <- fill_missing_cols(tbl, all_cols)
  all_stages_dataframe <- bind_rows(all_stages_dataframe, tbl)
}

# Arrange columns in the desired order
all_stages_dataframe <- all_stages_dataframe[, c(names(all_stages_dataframe)[order(match(names(all_stages_dataframe), all_cols))])]



### 4 - Read in pupil census ----

# Construct the stages list for the IN clause
sql_stages <- paste0(shQuote(all_stages, type = "sh"), collapse = ",")

# Construct the SQL query
query <- glue("SELECT ScottishCandidateNumber as 'pc_scn', 
               LaCode as pc_la, 
               StudentStage as pc_stage,
               StudentId,
               Gender,
               EthnicBackground,
               UrbRur6,
               Datazone2011
               FROM sch.student_{pupil_census_year}
               WHERE OnRoll = '1' AND
               StudentStage IN ({sql_stages}) AND
               SchoolFundingType IN ('2', '3')")

# Execute the query
pupil_census <- execute_sql(
  server = dbxed_server,
  database = "char_and_ref",
  sql = query,
  output = TRUE
)

# Apply lookup table to pupil_census to rename variables in column "LaCode" to match HWB
pupil_census$pc_la <- lookup_la_code[pupil_census$pc_la]



### 5 - Join pupil_census on to all_stages_dataframe ----

# Left join pupil_census onto all_stages_dataframe
joined_data <- left_join(all_stages_dataframe, pupil_census, by = c("hwb_scn" = "pc_scn"))

# Replace NA values in pc_stage column with "No stage match" in joined_data
joined_data <- joined_data %>%
  mutate(pc_stage = ifelse(is.na(pc_stage), "No stage match", pc_stage))
  
# Re-order columns
joined_data <- joined_data %>%
  select(pc_la, pc_stage, everything())



### 6 - Initialise removed_records as an empty dataframe, with the same column names as joined_data ----

# Records which have been removed from final_data_combined will get added to removed_records along with an explanation of why
removed_records <-
  joined_data %>%
  filter(FALSE) %>%
  mutate(reason_removed = "No reason provided") # Insert a new column "reason_removed"



### 7 - Create function to add rows to removed_records ----

# Function to add rows to removed_records
# Only add rows from new_dataframe to removed_records if that row does not currently exist in removed_records (i.e. stops a row getting added
# more than once if it is being rejected for multiple reasons)
update_removed_records <- function(existing_dataframe, new_dataframe, explanation_reason_removed) {
  if (nrow(new_dataframe) > 0) {
    new_rows <- anti_join(new_dataframe, existing_dataframe)
    if (nrow(new_rows) > 0) {
      new_rows <- new_rows %>%
        mutate(reason_removed = explanation_reason_removed)
      combined_dataframe <- bind_rows(existing_dataframe, new_rows)
    } else {
      combined_dataframe <- existing_dataframe
    }
  } else {
    combined_dataframe <- existing_dataframe
  }
  return(combined_dataframe)
}



### 8 - Remove records where a pupil answered the wrong stage questionnaire ----

# Create a dataframe called inconsistent_stages where hwb_stage != pc_stage
inconsistent_stages <- joined_data %>%
  filter(hwb_stage != pc_stage)

# Remove from inconsistent stages where
# hwb_stage = P5 & pc_stage = P6
# hwb_stage = P6 & pc_stage = P5
# hwb_stage = S5 & pc_stage = S6
# hwb_stage = S6 & pc_stage = S5
# This is because P5 & P6 pupils completed the same questionnaire, and S5 & S6 pupils completed the same questionnaire
inconsistent_stages <- inconsistent_stages %>%
  filter(!(hwb_stage == "P5" & pc_stage == "P6") & 
           !(hwb_stage == "P6" & pc_stage == "P5") &
           !(hwb_stage == "S5" & pc_stage == "S6") &
           !(hwb_stage == "S6" & pc_stage == "S5"))

# Insert these rows into removed_records, with reason_removed = "No stage match"
removed_records <- update_removed_records(removed_records, inconsistent_stages, "No stage match")



# 9 - Remove records where pupil census LA did not take part ----

# Create pc_la_didnt_take_part list
pc_la_didnt_take_part <- joined_data %>%
  filter(!pc_la %in% all_las)

# Insert these rows into removed_records, with reason_removed = "Pupil census LA did not take part"
removed_records <- update_removed_records(removed_records, pc_la_didnt_take_part, "Pupil census LA did not take part")



### 10 - Remove duplicate SCNs within each stage ----

# Create a modified copy of joined_data
# This is so we can temporarily pretend P5 & P6, and S5 & S6 pupils are in the same stage
# This is because P5 and P6 pupils completed the same questionnaire as well as S5 and S6 pupils
joined_data_copy <- joined_data %>%
  mutate(survey_completed = case_when(
    hwb_stage == "P5" ~ "P6",
    hwb_stage == "S5" ~ "S6",
    TRUE ~ hwb_stage
  ))

# Identify and extract rows with duplicated hwb_scn's
duplicate_scns_within_stage <- 
  joined_data_copy %>%
  get_dupes(hwb_scn, survey_completed)

# Remove the survey_completed column as now not needed
duplicate_scns_within_stage <- duplicate_scns_within_stage %>%
  select(-survey_completed, -dupe_count)

# Add a new column "number_questions_answered" to count non-missing values
duplicate_scns_within_stage <- duplicate_scns_within_stage %>%
  mutate(number_questions_answered = rowSums(!is.na(select(duplicate_scns_within_stage, -hwb_scn)) & 
                                               select(duplicate_scns_within_stage, -hwb_scn) != "Data not collected"))

# Remove EXACTLY ONE record for each hwb_scn from duplicate_scns_within_stage (this is the record which will be retained for analysis)
# Remove the record with the highest number in number_questions_answered
# If there is a tie, select which row to remove randomly using a seed (to make the code reproducible)
# Therefore re-running this code will always randomly remove the same records
# Do NOT change the number of this seed.

# Set a seed for reproducibility
set.seed(123)

duplicate_scns_within_stage <- duplicate_scns_within_stage %>%
  arrange(desc(number_questions_answered)) %>%  # Sort by number_questions_answered in descending order
  filter(duplicated(hwb_scn))

# Insert these rows into removed_records, with reason_removed = "Pupil completed multiple questionnaires within stage"
removed_records <- update_removed_records(removed_records, duplicate_scns_within_stage, "Pupil completed multiple questionnaires within stage")

# Remove East Renfrewshire records. This is because East Renfrewshire deliberately did not collect SCN so we have no way of validating or deduplicating their records
removed_records <- removed_records %>%
  filter(hwb_la != "East Renfrewshire")



### 11 - Save removed_records as an excel file to Output folder ----

# Save excel file to output folder
write_xlsx(
  removed_records,
  here("output", year, paste0(year, "_records_removed.xlsx"))
)



### 12 - Remove records in removed_records from joined_data ---- 

# Remove columns reason_removed, number_questions_answered from removed_records
removed_records <- removed_records %>%
  select(-reason_removed, -number_questions_answered)

# Identify rows in joined_data that appear more than once as some pupils completed the exact same survey. In this instance, we want them in final data ONCE and 
# in removed_records ALL BUT ONCE.
duplicated_rows <- joined_data[duplicated(joined_data) | duplicated(joined_data, fromLast = TRUE), ]
    
# Anti-join with removed_records to keep only rows in joined_data that are not in removed_records
result <- joined_data %>%
  anti_join(removed_records, by = colnames(joined_data)) %>%
  bind_rows(duplicated_rows %>% semi_join(removed_records, by = colnames(joined_data)))
    
# Keep only the first occurrence of each unique row when hwb_la != "East Renfrewshire"
joined_data_removed <- result[!(duplicated(result) & result$hwb_la != "East Renfrewshire"), ]

# There was an odd case where one pupil completed four records, two of which were identical. Those two identical records and one other
# needed to be removed but the prior code isn't set up to handle that, but it's set up to handle the case where there were one pupil completed
# the exact same response twice, so they should appear once in removed_records and once in final_data_combined_filtered.
# Function to remove duplicates based on specified conditions
joined_data_removed <- joined_data_removed %>%
  filter(hwb_scn != "Data not collected") %>%  # Exclude rows where hwb_scn is "Data not collected"
  group_by(hwb_scn) %>%
  arrange(rowSums(is.na(.) | . == "Data not collected")) %>%
  slice(1) %>%
  ungroup()



### 13 - Add East Renfrewshire records back in ----

# Get dataframe of East Renfrewshire records
east_ren <- all_stages_dataframe %>%
  filter(hwb_la == "East Renfrewshire")

# Add columns pc_la and pc_stage to east_ren
# These will be the same as hwb_la and hwb_stage because East Renfrewshire didn't collect scn so we have no way of linking to the pupil census
east_ren <- east_ren %>%
  mutate(pc_la = hwb_la,
         pc_stage = hwb_stage)

# Add East Renfrewshire records to joined_data_removed
joined_data_removed <- bind_rows(joined_data_removed, east_ren)

# Replace hwb_scn with "Data not collected" where pc_la = "East Renfrewshire"
# This is because if a pupil completed the survey in a LA that wasn't East Renfrewshire, and then moved to East Renfrewshire, then
# we want to include them as East Renfrewshire for the published analysis, but we don't publish any characteristic breakdowns for 
# East Renfrewshire as they didn't collect SCN.
joined_data_removed <- joined_data_removed %>%
  mutate(hwb_scn = ifelse(pc_la == "East Renfrewshire", "Data not collected", hwb_scn))



### 14 - Save joined_data_removed as an excel file to Merged folder ----

# Save excel file to output folder
write_xlsx(
  joined_data_removed,
  file.path(raw_data_folder, year, "Merged", paste0("08_removed_records.xlsx"))
)


### END OF SCRIPT ###
