#########################################################################
# Name of file - 10_remove_rows.R
# Health and Wellbeing (HWB) Census 
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 4.2.2
#
# Description - This code removes records for pupils who answered the 
# wrong stage questionnaire or who responded more than once
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
pupil_census <- dbGetQuery(myconn, paste(paste0("SELECT ScottishCandidateNumberN,
                                                LaCode,
                                                StudentStage
                                                FROM sch.student_", pupil_census_year),
                                                "WHERE OnRoll = '1' AND
                                                StudentStage IN ('P5', 'P6', 'P7',
                                                'S1', 'S2', 'S3', 'S4', 'S5', 'S6') AND
                                                SchoolFundingType IN ('2', '3')"))

test <- pupil_census %>%
  filter(ScottishCandidateNumberN == "90596373")

# Get this working later
pupil_census2 <- dbGetQuery(myconn, paste(paste0("SELECT ScottishCandidateNumberN,
                                                LaCode,
                                                StudentStage
                                                FROM sch.student_", pupil_census_year),
                                                "WHERE OnRoll = '1' AND
                                                StudentStage IN ('", paste(all_stages, collapse = "','"), "') AND
                                                SchoolFundingType IN ('2', '3')"))



### 3 - Prepare raw_data for joining ----

# Add "hwb_stage" column at the beginning of each tibble with respective stage (taken from tibble names)
modified_data <- map2(raw_data, names(raw_data), ~ .x %>% 
                   mutate(hwb_stage = .y) %>% 
                   relocate(hwb_stage, .before = 1))

# Rename column "scn" to "hwb_scn" for ease of comparison
modified_data <- lapply(modified_data, function(tibble) {
    tibble <- dplyr::rename(tibble, hwb_scn = scn)
})

# Remove leading 0's in hwb_scn. This is because pupil_census does this so we need to be able to match hwb scn's to pupil census scn's
modified_data <- lapply(modified_data, function(df) {
  df %>%
    mutate(hwb_scn = ifelse(nchar(hwb_scn) == 9 & grepl("^0", hwb_scn),
                            as.character(as.numeric(hwb_scn)), hwb_scn))
})

test <- modified_data$S3 %>%
  filter(hwb_scn == "90596373")



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

test <- pupil_census %>%
  filter(pc_scn == "90596373")

### 5 - Join pupil_census on to modified_data ----

# Define a function to left join pupil_census onto a tibble, only when hwb_scn is a 8 or 9 digit number (should be 9 digit number but 
# pupil census removes 0s at the start)
left_join_census <- function(tibble) {
  
  tibble_filtered <- tibble %>%
    filter(grepl("^[0-9]{8,9}$", hwb_scn))
  
  result <- left_join(tibble_filtered, pupil_census, by = c("hwb_scn" = "pc_scn"))
  
  return(result)
}

# Apply left_join_census function to each tibble in modified_data
joined_data <- map(modified_data, left_join_census)

test <- joined_data$S3 %>%
  filter(hwb_scn == "90596373")
test2 <- raw_data$S4 %>%
  filter(scn == "90596306")




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

# Group tibbles by their 'name' column and combine them into one tibble each
final_data_combined <- final_data %>%
  bind_rows() %>%   # Combine all tibbles into one
  group_split(pc_stage)  # Split tibbles into a list by 'name'


# Create a unique list of tibble names
tibble_names <- unique(names(final_data))

# Initialize an empty list to store the combined tibbles
final_data_combined <- list()

# Loop through each unique tibble name and combine the corresponding tibbles
for (name in tibble_names) {
  tibbles_to_combine <- final_data[names(final_data) == name]
  combined_tibble <- do.call(bind_rows, tibbles_to_combine)
  final_data_combined[[name]] <- combined_tibble
}

# Re-order tibbles in final_data_combined to alphabetical (so it goes like P5, P6, P7, S1, S2,... rather than P5, P6, P7, S3, ...)
final_data_combined <- final_data_combined[order(names(final_data_combined))]


# Remove any columns after pc_stage
# This is because re-arranging the tibbles by pc_stage will have created some extra columns for pupils that did the wrong stage survey
# All of these rows 



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




# ### 8 - Remove records which are exactly the same ---
# 
# # Create a list of tibbles called exact_same_rows
# exact_same_rows <- lapply(final_data_combined, function(tibble) {
#   # Find duplicate rows in the current tibble
#   duplicates <- tibble[duplicated(tibble) | duplicated(tibble, fromLast = TRUE), ]
#   return(duplicates)
# })
# 
# # Remove duplicates
# exact_same_rows <- lapply(exact_same_rows, function(tibble) {
#   tibble %>%
#     distinct()
# })
# 
# Insert these rows into removed_records, with reason_removed = "Exactly the same as another record"
# Function to update tibbles in removed_records
update_tibble <- function(existing_tibble, new_tibble, explanation_reason_removed) {
  if (nrow(new_tibble) > 0) {
    new_tibble <- new_tibble %>%
      mutate(reason_removed = explanation_reason_removed)
    combined_tibble <- bind_rows(existing_tibble, new_tibble)
  } else {
    combined_tibble <- existing_tibble
  }
  return(combined_tibble)
}
# 
# # Update the tibbles in removed_records with rows from exact_same_rows
# removed_records <- mapply(update_tibble, removed_records, exact_same_rows, "Exactly the same as another record")


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
# Update the tibbles in removed_records with rows from inconsistent_stages
removed_records <- mapply(update_tibble, removed_records, inconsistent_stages, "Pupil completed different stage questionnaire")

test2 <- removed_records$S1 %>%
  filter(hwb_scn == "150854091")




# 10 - Remove records where pupil census LA did not take part ---

# Create pc_la_didnt_take_part list
pc_la_didnt_take_part <- lapply(final_data_combined, function(tibble_data) {
  tibble_data %>% filter(!pc_la %in% all_las)
})

test2 <- pc_la_didnt_take_part$P5 %>%
  filter(hwb_scn == "220075664")


# Insert these rows into removed_records, with reason_removed = "Pupil census LA did not take part"
# Update the tibbles in removed_records with rows from pc_la_didnt_take_part
removed_records <- mapply(update_tibble, removed_records, pc_la_didnt_take_part, "Pupil census LA did not take part")



### 11 - Remove duplicate scns within each stage ---

# Create an empty named list to store the results
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
test <- duplicate_scns_within_stage$S1 %>%
  filter(hwb_scn == "150854091")

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
#    distinct(hwb_scn, .keep_all = TRUE)  # Keep only the first row for each hwb_scn
  
  # Assign the modified tibble back to the list
  duplicate_scns_within_stage[[tibble_name]] <- tibble
}

# Insert these rows into removed_records, with reason_removed = "Pupil completed multiple questionnaires within stage"
# Update the tibbles in removed_records with rows from duplicate_scns_within_stage
removed_records <- mapply(update_tibble, removed_records, duplicate_scns_within_stage, "Pupil completed multiple questionnaires within stage")





# 12 - Remove records with invalid SCNs (excluding any LAs that did not collect SCN, i.e. have hwb_scn = "Data not collected") ---
# NOTE here we are using modified_data (unjoined to pupil census as these are records which would have an invalid scn so wouldn't match)

# Define a function to filter tibbles within the list
filter_invalid_scns <- function(tibble) {
  tibble %>%
    filter(hwb_scn != "Data not collected" & !grepl("^\\d{9}$", hwb_scn))
}

# Use lapply to apply the filter_tibble function to each tibble in modified_data
invalid_scns <- lapply(modified_data, filter_invalid_scns)

# Insert these rows into removed_records, with reason_removed = "Invalid SCN"
# Update the tibbles in removed_records with rows from invalid_scns
removed_records <- mapply(update_tibble, removed_records, invalid_scns, "Invalid SCN")



# 13 - Remove records with blank SCNs ---

# Create an empty list to store the results
blank_scns <- list()

# Loop through each tibble in modified_data
for (i in seq_along(modified_data)) {
  # Filter rows where 'hwb_scn' is NA
  filtered_tibble <- modified_data[[i]][is.na(modified_data[[i]]$hwb_scn), ]
  
  # Assign the filtered tibble to blank_scns with the same name
  tbl_name <- names(modified_data)[i]
  blank_scns[[tbl_name]] <- filtered_tibble
}

# Insert these rows into removed_records, with reason_removed = "Blank SCN"
# Update the tibbles in removed_records with rows from blank_scns
removed_records <- mapply(update_tibble, removed_records, blank_scns, "Blank SCN")



# 14 - Remove records with no SCN match to the pupil census (excluding any LAs that did not collect SCN) ---
# Create a list of tibbles called scns_without_pc_match (where the names of the tibbles are the same as the names of the tibbles 
# in modified_data). The rows contained in scns_without_pc_match are the rows in modified_data that have a 9 digit number in the column 
# hwb_scn, that doesn't contain a match in the pc_scn column of pupil_census.

# Create an empty list to store the results
scns_without_pc_match <- list()

# Iterate through each tibble in modified_data
for (i in seq_along(modified_data)) {
  # Extract the tibble
  tibble <- modified_data[[i]]
  
  # Filter rows with 9-digit numbers in hwb_scn that don't match pc_scn in pupil_census
  filtered_tibble <- tibble[grepl("^[0-9]{9}$", tibble$hwb_scn) & !tibble$hwb_scn %in% pupil_census$pc_scn, ]
  
  # Assign the filtered tibble to the corresponding name in the result list
  name <- names(modified_data)[i]
  scns_without_pc_match[[name]] <- filtered_tibble
}

# Insert these rows into removed_records, with reason_removed = "No SCN match in pupil census"
# Update the tibbles in removed_records with rows from scns_without_pc_match
removed_records <- mapply(update_tibble, removed_records, scns_without_pc_match, "No SCN match in pupil census")

test2 <- removed_records$S1 %>%
  filter(hwb_scn == "150854091")


# 15 - Remove duplicate rows in removed_records --- 

# Is this the right approach??? SCN 150854091 in S1 completed four surveys,
# two of which were the same and were removed, two individual ones (one of which was correct)
# this approach changes 3 removed questionnaires to 2 (incorrect) so what will changing it do??

# I guess not needed as it's only that record that's getting de-duplicated!

# # Use the map function to remove duplicate rows in each tibble
# removed_records_no_duplicates <- map(removed_records, ~distinct(.x))
# 
# test <- removed_records_no_duplicates$S1 %>%
#   filter(hwb_scn == "150854091")



# 16 - Save removed_records as an excel file to Output folder --- 

# Save excel file to output folder
write_xlsx(
  removed_records, 
  here("output", year, paste0(year, "_records_removed.xlsx"))
)

test3 <- removed_records$S1 %>%
  filter(hwb_scn == "150854091")



# 17 - Remove records in removed_records from final_data_combined --- 

# Remove columns pc_la, pc_stage, reason_removed, number_questions_answered
removed_records <- map(removed_records, 
                                     ~ select(.x, -reason_removed, -number_questions_answered))

hwb_scn <- "hwb_scn"

# Function to sort tibbles based on hwb_scn
sort_tibble <- function(df) {
  df %>%
    arrange({{hwb_scn}})
}

# Sort both data frames
sorted_final_data_combined <- lapply(final_data_combined, sort_tibble)
sorted_removed_records <- lapply(removed_records, sort_tibble)

# final_data_combined_filtered <- setNames(
#   lapply(names(sorted_final_data_combined), function(name) {
#     df <- sorted_final_data_combined[[name]]
#     removed <- sorted_removed_records[[name]]
#     
#     result <- df %>%
#       anti_join(removed, by = colnames(df))
#     
#     # Keep only the first occurrence of each unique row
#     result[!duplicated(result), ]
#   }),
#   names(sorted_final_data_combined)
# )

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


test <- final_data_combined_filtered$S1 %>%
  filter(hwb_scn == "150854091")



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




### 18 - Add East Renfrewshire records back in ---
east_ren <- map(modified_data, ~filter(.x, hwb_la == "East Renfrewshire"))

# Add columns pc_la and pc_stage to east_ren
# These will be the same as hwb_la and hwb_stage because East Renfrewshire didn't collect scn so we have no way of linking to the pupil census
east_ren <- map(east_ren, ~mutate(.x, pc_la = hwb_la, pc_stage = hwb_stage))

combined_data <- Map(function(x, y) bind_rows(x, y), final_data_combined_filtered, east_ren)

unique(combined_data$S4$pc_la)


### 20 - Replace hwb_scn with "Data not collected" where pc_la = "East Renfrewshire" --- 
# Define a function to replace values in hwb_scn column
# replace_hwb_scn <- function(tibble) {
#   tibble %>% 
#     mutate(hwb_scn = ifelse(pc_la == "East Renfrewshire", "Data not collected", hwb_scn))
# }
# 
# # Use lapply to apply the function to each tibble in the list
# final_data_combined <- lapply(final_data_combined, replace_hwb_scn)
#
# Not sure if this needs doing - revisit



### 19 - Re-order columns to match what they originally were and remove blank columns

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




### 20 - Save final_data_combined_filtered as an excel file to Merged folder --- 

write_xlsx(
  cleaned_combined_data_reordered,
  file.path(raw_data_folder, year, "Merged", paste0("08_removed_records.xlsx"))
)

testt <- cleaned_combined_data_reordered$S4 %>%
  filter(hwb_scn == "090596306")
  






# Accessing the S3 tibble
s3_tibble <- cleaned_combined_data_reordered$S3

# Finding duplicates in the column hwb_scn
duplicates <- s3_tibble[duplicated(s3_tibble$hwb_scn) | duplicated(s3_tibble$hwb_scn, fromLast = TRUE), ]

# Creating the dataframe test containing duplicates
test <- as.data.frame(duplicates)


