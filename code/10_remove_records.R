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
# be linked to the pupil census for analysis.
#########################################################################


### 0 - Setup ----

source(here::here("code", "00_setup.R"))
source(here::here("functions", "read_raw_data.R"))
source(here::here("lookups", "lut_la_code.R"))



### 1 - Read in raw data ----

# Define the path to Excel file
file_path <- file.path(raw_data_folder, year, "Merged", "07_validated_school_names.xlsx")

# Read in all sheets into a list of tibbles with names
raw_data <- set_names(
  map(
    all_stages,
    ~ read_xlsx(file_path, sheet = .x)
  ),
  all_stages
)



### 2 - Read in pupil census ----

# Load and select required variables from latest pupil census table 
myconn <- dbConnect(odbc::odbc(), "dbXed", UID="", PWD= "")

# Construct the placeholders for the IN clause
placeholders <- paste(rep("?", length(all_stages)), collapse = ",")

# Construct the SQL query with parameterized IN clause
query <- glue::glue("SELECT ScottishCandidateNumberN, LaCode, StudentStage
                     FROM sch.student_{pupil_census_year}
                     WHERE OnRoll = '1' AND
                     StudentStage IN ({placeholders}) AND
                     SchoolFundingType IN ('2', '3')")

# Execute the query using parameterized inputs
pupil_census <- dbGetQuery(myconn, query, params = all_stages)



### 3 - Prepare raw_data for joining ----

# Add "hwb_stage" column at the beginning of each tibble with respective stage (taken from tibble names)
modified_data <- map2(raw_data, names(raw_data), ~ .x %>% 
                   mutate(hwb_stage = .y) %>% 
                   relocate(hwb_stage, .before = 1))

# Rename column "scn" to "hwb_scn" for ease of comparison to the pupil census
modified_data <- lapply(modified_data, function(tibble) {
    tibble <- dplyr::rename(tibble, hwb_scn = scn)
})



### 4 - Prepare pupil_census for joining ----

# Apply lookup table to pupil_census to rename variables in column "LaCode"
pupil_census$LaCode <- lut_la_code[pupil_census$LaCode]

# Rename column "ScottishCandidateNumberN" to "pc_scn"
# Rename column "LaCode" to "pc_la"
# Rename column "StudentStage" to "pc_stage"
pupil_census <- pupil_census %>%
  rename(pc_scn = ScottishCandidateNumberN, 
         pc_la = LaCode,
         pc_stage = StudentStage)

# Convert column "pc_scn" from numeric to character
pupil_census$pc_scn <- as.character(pupil_census$pc_scn)



### 5 - Join pupil_census on to modified_data ----

# Define a function to left join pupil_census onto a tibble, only when hwb_scn is a 8 or 9 digit number (should be 9 digit number but 
# pupil census leading 0s at the start)
# Also need to match like 012345678 from hwb to 12345678 in pupil census
left_join_census <- function(tibble) {
  
  tibble_filtered <- tibble %>%
    filter(grepl("^[0-9]{8,9}$", hwb_scn)) %>%
    mutate(hwb_scn_cleaned = as.numeric(hwb_scn)) # Temporary numeric version for join
  
  pupil_census_cleaned <- pupil_census %>%
    mutate(pc_scn_cleaned = as.numeric(pc_scn)) # Temporary numeric version for join
  
  result <- tibble_filtered %>%
    left_join(pupil_census_cleaned, by = c("hwb_scn_cleaned" = "pc_scn_cleaned"))
  
  return(result)
}

# Apply left_join_census function to each tibble in modified_data
joined_data <- map(modified_data, left_join_census)

# Replace NA values in pc_stage column with "No stage match" string in each tibble
joined_data <- map(joined_data, ~ mutate(.x, pc_stage = ifelse(is.na(pc_stage), "No stage match", pc_stage)))

# Remove columns "hwb_scn_cleaned" and "pc_scn" from every tibble in joined_data
joined_data <- map(joined_data, ~ .x %>% select(-c(hwb_scn_cleaned, pc_scn)))
  


### 6 - Re-organise tibbles by pc_stage  --- 
# Instead of organising tibbles by hwb_stage, organise by pc_stage
# This is because analysis by stage will be carried out by pc_stage

# Create an empty list to store the final result
final_data <- list()

# Loop through each tibble in joined_data
for(tibble_df in joined_data) {
  # Split the tibble into a list of tibbles based on the pc_stage column
  split_tibbles <- split(tibble_df, tibble_df$pc_stage)
  
  # Merge the split tibbles into final_data
  final_data <- append(final_data, split_tibbles)
}

# Create a unique list of tibble names, 15.11 this should be a unique list of values in pc_stage column of every tibble in joined data!
# Missing records else
tibble_names <- unique(names(final_data))

# Initialize an empty list to store the combined tibbles
final_data_combined <- list()

# Loop through each unique tibble name and combine the corresponding tibbles
for (name in tibble_names) {
  tibbles_to_combine <- final_data[names(final_data) == name]
  combined_tibble <- do.call(bind_rows, tibbles_to_combine)
  final_data_combined[[name]] <- combined_tibble
}

# Re-order tibbles in final_data_combined to alphabetical (so it goes like P5, P6, P7, S1, S2,... rather than P5, P7, P6, S3, ...)
final_data_combined <- final_data_combined[order(names(final_data_combined))]



### 7 - Initialise removed_records as an empty list of tibbles ---

# Records which have been removed from final_data_combined will get added to removed_records along with an explanation of why
# Create a function that removes all rows from a tibble
remove_rows <- function(tibble) {
  tibble %>% filter(FALSE)
}

# Use lapply to apply the function to each tibble in final_data_combined
removed_records <- lapply(final_data_combined, remove_rows)

# Function to insert a new column "reason_removed" into a tibble
add_reason_removed_column <- function(tibble) {
  tibble %>% mutate(reason_removed = "No reason provided")
}

# Use lapply to apply the function to each tibble in removed_records
removed_records <- lapply(removed_records, add_reason_removed_column)



# ### 8 - Create function to add rows to removed_records ---

# Function to update tibbles in removed_records
# Only add rows from new_tibble to removed_records if that row does not currently exist in removed_records (i.e. stops a row getting added
# more than once if it is being rejected for multiple reasons)
update_tibble <- function(existing_tibble, new_tibble, explanation_reason_removed) {
  if (nrow(new_tibble) > 0) {
    new_rows <- anti_join(new_tibble, existing_tibble)
    if (nrow(new_rows) > 0) {
      new_rows <- new_rows %>%
        mutate(reason_removed = explanation_reason_removed)
      combined_tibble <- bind_rows(existing_tibble, new_rows)
    } else {
      combined_tibble <- existing_tibble
    }
  } else {
    combined_tibble <- existing_tibble
  }
  return(combined_tibble)
}



### 9 - Remove records where a pupil answered the wrong stage questionnaire ---

# Create a list of tibbles called inconsistent_stages where hwb_stage != pc_stage

# Create a function to filter each tibble in the final_data_combined list
filter_inconsistent_stages <- function(tibble) {
  return(tibble %>% filter(hwb_stage != pc_stage))
}

# Apply the function to each tibble in the final_data_combined list using map()
inconsistent_stages <- map(final_data_combined, filter_inconsistent_stages)

# Remove from inconsistent stages where
# hwb_stage = P5 & pc_stage = P6
# hwb_stage = P6 & pc_stage = P5
# hwb_stage = S5 & pc_stage = S6
# hwb_stage = S6 & pc_stage = S5
# This is because P5 & P6 pupils completed the same questionnaire, and S5 & S6 pupils completed the same questionnaire
# Other stages completed a different, age-appropriate questionnaire so there records must be removed

# Create a function to filter out the rows with the specified conditions
filter_rows <- function(tibble) {
  tibble %>%
    filter(!(hwb_stage == "P5" & pc_stage == "P6") & 
             !(hwb_stage == "P6" & pc_stage == "P5") &
             !(hwb_stage == "S5" & pc_stage == "S6") &
             !(hwb_stage == "S6" & pc_stage == "S5"))
}

# Apply the function to each tibble in the list
inconsistent_stages <- lapply(inconsistent_stages, filter_rows)

# Insert these rows into removed_records, with reason_removed = "Pupil completed different stage questionnaire"
removed_records <- mapply(update_tibble, removed_records, inconsistent_stages, "Pupil completed different stage questionnaire")



# 10 - Remove records where pupil census LA did not take part ---

# Create pc_la_didnt_take_part list
pc_la_didnt_take_part <- lapply(final_data_combined, function(tibble_data) {
  tibble_data %>% filter(!pc_la %in% all_las)
})

# Insert these rows into removed_records, with reason_removed = "Pupil census LA did not take part"
removed_records <- mapply(update_tibble, removed_records, pc_la_didnt_take_part, "Pupil census LA did not take part")



### 11 - Remove duplicate scns within each stage ---

# Create an empty list to store the results
duplicate_scns_within_stage <- vector("list", length(final_data_combined))
names(duplicate_scns_within_stage) <- names(final_data_combined)

# Loop through each tibble in the final_data_combined list
for (tibble_name in names(final_data_combined)) {
  tibble <- final_data_combined[[tibble_name]]
  # Group the tibble by hwb_scn column
  temp <- tibble %>%
    group_by(hwb_scn) %>%
    # Filter rows with duplicate hwb_scn values
    filter(n() > 1) %>%
    # Ungroup the tibble
    ungroup()
  
  # Assign the result to the corresponding name in the duplicate_scns_within_stage list
  duplicate_scns_within_stage[[tibble_name]] <- temp
}

# Create a new column in each tibble called number_questions_answered
# Loop through each tibble in duplicate_scns_within_stage
for (tibble_name in names(duplicate_scns_within_stage)) {
  tibble <- duplicate_scns_within_stage[[tibble_name]]
  
  # Add a new column "number_questions_answered" to count non-missing values
  tibble <- tibble %>%
    mutate(number_questions_answered = rowSums(!is.na(select(tibble, -hwb_scn)) & select(tibble, -hwb_scn) != "Data not collected"))
  
  # Assign the modified tibble back to the list
  duplicate_scns_within_stage[[tibble_name]] <- tibble
}

# Remove EXACTLY ONE record for each hwb_scn from duplicate_scns_within_stage (this is the record which will be retained for analysis)
# Remove the record with the highest number in number_questions_answered
# If there is a tie, select which row to remove randomly using a seed (to make the code reproducible)
# Therefore re-running this code will always randomly remove the same records
# Do NOT change the number of this seed.

# Set a seed for reproducibility
set.seed(123)

# Loop through each tibble in duplicate_scns_within_stage
for (tibble_name in names(duplicate_scns_within_stage)) {
  tibble <- duplicate_scns_within_stage[[tibble_name]]
  
  tibble <- tibble %>%
    arrange(desc(number_questions_answered)) %>%  # Sort by number_questions_answered in descending order
    filter(duplicated(hwb_scn))
  
  # Assign the modified tibble back to the list
  duplicate_scns_within_stage[[tibble_name]] <- tibble
}

# Insert these rows into removed_records, with reason_removed = "Pupil completed multiple questionnaires within stage"
removed_records <- mapply(update_tibble, removed_records, duplicate_scns_within_stage, "Pupil completed multiple questionnaires within stage")

# Create a new tibble in modified_data for 'No stage match', copying the format of the P5 tibble
if ("P5" %in% names(modified_data)) {
  headers <- colnames(modified_data$P5)
  
  # Create an empty dataframe with the same headers
  new_tibble <- data.frame(matrix(NA, nrow = 0, ncol = length(headers)))
  colnames(new_tibble) <- headers
  
  # Insert this empty dataframe into modified_data list as the first element
  modified_data <- c(list("No stage match" = new_tibble), modified_data)
} else {
  # If "P5" dataframe doesn't exist, create an empty dataframe with no headers
  new_tibble <- data.frame()
  
  # Insert an empty dataframe as "No stage match" as the first element
  modified_data <- c(list("No stage match" = new_tibble), modified_data)
}



# 12 - Remove records with invalid SCNs (excluding any LAs that did not collect SCN, i.e. have hwb_scn = "Data not collected") ---
# NOTE here we are using modified_data (unjoined to pupil census as these are records which would have an invalid scn so wouldn't match)

# Define a function to filter tibbles within the list
filter_invalid_scns <- function(tibble) {
  tibble %>%
    filter(hwb_scn != "Data not collected" & !grepl("^\\d{8,9}$", hwb_scn))
}

# Use lapply to apply the filter_tibble function to each tibble in modified_data
invalid_scns <- lapply(modified_data, filter_invalid_scns)

# Insert these rows into removed_records, with reason_removed = "Invalid SCN"
removed_records <- mapply(update_tibble, removed_records, invalid_scns, "Invalid SCN")



### 13 - Remove records with no SCN match to the pupil census (excluding any LAs that did not collect SCN) ---
# Create a list of tibbles called scns_without_pc_match (where the names of the tibbles are the same as the names of the tibbles 
# in modified_data). The rows contained in scns_without_pc_match are the rows in modified_data that have a 8 or 9 digit number in the column 
# hwb_scn, that doesn't contain a match in the pc_scn column of pupil_census.

# Create an empty list to store the results
scns_without_pc_match <- list()

# Iterate through each tibble in modified_data
for (i in seq_along(modified_data)) {
  # Extract the tibble
  tibble <- modified_data[[i]]
  
  # Filter rows with 9-digit numbers in hwb_scn that don't match pc_scn in pupil_census
  filtered_tibble <- tibble[grepl("^[0-9]{8,9}$", tibble$hwb_scn) & !tibble$hwb_scn %in% pupil_census$pc_scn, ]
  
  # Assign the filtered tibble to the corresponding name in the result list
  name <- names(modified_data)[i]
  scns_without_pc_match[[name]] <- filtered_tibble
}

# Insert these rows into removed_records, with reason_removed = "No SCN match in pupil census"
# Update the tibbles in removed_records with rows from scns_without_pc_match
removed_records <- mapply(update_tibble, removed_records, scns_without_pc_match, "No SCN match in pupil census")

# Remove rows from removed_records in P5-S6 where pc_stage is NA. This is because they appear twice, once in tibble 'No stage match'
# and once in the stage tibble of their hwb_stage
removed_records <- imap(removed_records, function(tbl, tbl_name) {
  if (tbl_name %in% all_stages) {
    filtered_tbl <- tbl %>%
      filter(!is.na(pc_stage))
    return(filtered_tbl)
  } else {
    return(tbl)
  }
})



### 14 - Save removed_records as an excel file to Output folder --- 

# Save excel file to output folder
write_xlsx(
  removed_records, 
  here("output", year, paste0(year, "_records_removed.xlsx"))
)



### 15 - Remove records in removed_records from final_data_combined --- 

# Remove pc_la and pc_stage columns from each tibble in joined_data
joined_data2 <- lapply(joined_data, function(tib) {
  tib %>% 
    select(-pc_la, -pc_stage)
})

# Insert blank tibble called 'No stage match' in joined_data2 to match the structure of modified_data
# Get the headers/column names of the P5 tibble (assuming it exists)
if ("P5" %in% names(joined_data2)) {
  headers <- colnames(joined_data2$P5)
  
  # Create an empty tibble with the same headers
  new_tibble <- tibble::tibble(!!!setNames(rep(list(NA), length(headers)), headers))
  
  # Insert this empty tibble into joined_data2 list as the first element
  joined_data2 <- c(list("No stage match" = new_tibble), joined_data2)
} else {
  # If "P5" tibble doesn't exist, create an empty tibble with no headers
  new_tibble <- tibble::tibble()
  
  # Insert an empty tibble as "No stage match" as the first element
  joined_data2 <- c(list("No stage match" = new_tibble), joined_data2)
}


# Function to perform anti-join for each tibble name
get_unjoined_data <- function(modified_tbl, joined_tbl) {
  unjoined_tbl <- Map(function(mod_tbl, joined_tbl) {
    anti_join(mod_tbl, joined_tbl)
  }, modified_tbl, joined_tbl)
  names(unjoined_tbl) <- names(modified_tbl)  # Preserve original tibble names
  return(unjoined_tbl)
}

# Creating unjoined_data list of tibbles
unjoined_data <- get_unjoined_data(modified_data, joined_data2)

# Remove columns reason_removed, number_questions_answered
removed_records <- lapply(removed_records, function(tbl) {
  if ("reason_removed" %in% colnames(tbl)) {
    tbl <- tbl %>% select(-reason_removed)
  }
  if ("number_questions_answered" %in% colnames(tbl)) {
    tbl <- tbl %>% select(-number_questions_answered)
  }
  return(tbl)
})

hwb_scn <- "hwb_scn"

# Function to sort tibbles based on hwb_scn
sort_tibble <- function(df) {
  df %>%
    arrange({{hwb_scn}})
}

# Sort both data frames
sorted_final_data_combined <- lapply(final_data_combined, sort_tibble)
sorted_removed_records <- lapply(removed_records, sort_tibble)

final_data_combined_filtered <- setNames(
  lapply(names(sorted_final_data_combined), function(name) {
    df <- sorted_final_data_combined[[name]]
    removed <- sorted_removed_records[[name]]
    
    # Identify rows in df that appear more than once
    # As some pupils completed the exact same survey
    duplicated_rows <- df[duplicated(df) | duplicated(df, fromLast = TRUE), ]
    
    # Anti-join with removed to keep only rows in df that are not in removed
    result <- df %>%
      anti_join(removed, by = colnames(df)) %>%
      bind_rows(duplicated_rows %>% semi_join(removed, by = colnames(df)))
    
    # Keep only the first occurrence of each unique row
    result[!duplicated(result), ]
  }),
  names(sorted_final_data_combined)
)



# Check for duplicates in each tibble within the list
for (i in seq_along(final_data_combined_filtered)) {
  tbl <- final_data_combined_filtered[[i]]
  duplicates <- duplicated(tbl$hwb_scn)
  
  # Print the table index if duplicates are found
  if (any(duplicates)) {
    print(paste("Duplicates found in table:", i))
  }
}

## scn 150854091 in S1 is a very odd case where they completed four records, two of which were identical. Those two identical records and
## one other needed to be removed but the prior code isn't set up to handle that, but it's set up to handle the case where there were one
## pupil completed the exact same response twice in S1, so they should appear once in removed_records and once in final_data_combined_filtered
# Function to remove duplicates based on specified conditions
remove_duplicates <- function(df) {
  df %>%
    group_by(hwb_scn) %>%
    arrange((rowSums(is.na(.) | . == "Data not collected"))) %>%
    slice(1) %>%
    ungroup()
}

# Apply the function to each tibble in the list
final_data_combined_filtered <- lapply(final_data_combined_filtered, remove_duplicates)




### 16 - Add East Renfrewshire records back in ---

# Remove tibble 'No stage match' from final_data_combined_filtered
final_data_combined_filtered <- final_data_combined_filtered[!names(final_data_combined_filtered) %in% 'No stage match']

east_ren <- map(modified_data, ~filter(.x, hwb_la == "East Renfrewshire"))

east_ren <- east_ren[!names(east_ren) %in% 'No stage match']

# Add columns pc_la and pc_stage to east_ren
# These will be the same as hwb_la and hwb_stage because East Renfrewshire didn't collect scn so we have no way of linking to the pupil census
east_ren <- map(east_ren, ~mutate(.x, pc_la = hwb_la, pc_stage = hwb_stage))

combined_data <- Map(function(x, y) bind_rows(x, y), final_data_combined_filtered, east_ren)

unique(combined_data$S4$pc_la)



### 17 - Replace hwb_scn with "Data not collected" where pc_la = "East Renfrewshire" --- 

# This is because if a pupil completed the survey in a LA that wasn't East Renfrewshire, and then moved to East Renfrewshire, then
# we want to include them as East Renfrewshire for the published analysis, but we don't publish any characteristic breakdowns for 
# East Renfrewshire as they didn't collect SCN.

# Define a function to replace values in hwb_scn column with "Data not collected" when the pupil census local authority is East Renfrewshire
replace_hwb_scn <- function(tibble) {
  tibble %>%
    mutate(hwb_scn = ifelse(pc_la == "East Renfrewshire", "Data not collected", hwb_scn))
}

# Use lapply to apply the function to each tibble in the list
final_data_combined <- lapply(final_data_combined, replace_hwb_scn)



### 18 - Re-order columns to match what they originally were and remove blank columns

# Remove blank columns, these were introduced when re-organising tibbles from hwb_stage into pc_stage
# Function to remove blank columns from a tibble
remove_blank_columns <- function(tibble_data) {
  tibble_data %>%
    select_if(~any(!is.na(.)))
}

# Apply the function to each tibble in combined_data
cleaned_combined_data <- combined_data %>%
  map(remove_blank_columns)

# Re-order columns to match what they originally were
# Function to reorder columns in cleaned_combined_data to match joined_data
reorder_columns <- function(joined_data, cleaned_combined_data) {
  # Loop through each tibble name in the joined_data
  for (tibble_name in names(joined_data)) {
    # Get the column order from joined_data
    column_order <- names(joined_data[[tibble_name]])
    
    # Reorder columns in cleaned_combined_data to match column_order
    cleaned_combined_data[[tibble_name]] <- cleaned_combined_data[[tibble_name]][, column_order, drop = FALSE]
  }
  
  return(cleaned_combined_data)
}

# Call the function
cleaned_combined_data_reordered <- reorder_columns(joined_data, cleaned_combined_data)



### 19 - Save final_data_combined_filtered as an excel file to Merged folder --- 

write_xlsx(
  cleaned_combined_data_reordered,
  file.path(raw_data_folder, year, "Merged", paste0("08_removed_records.xlsx"))
)



### END OF SCRIPT ###







