#########################################################################
# Name of file - 08_validate_school_name.R
# Health and Wellbeing (HWB) Census 
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 4.2.2
#
# Description - Splits the column school_name into two columns, school_name and seed_code

#########################################################################


### 0 - Setup ----

source(here::here("code", "00_setup.R"))
source(here::here("functions", "read_raw_data.R"))



### 1 - Read in raw data ----

# Define the path to Excel file
file_path <- file.path(raw_data_folder, year, "Merged", "06_validated_routing.xlsx")

# Read in all sheets into a list of tibbles with names
raw_data <- set_names(
  map(
    all_stages,
    ~ read_xlsx(file_path, sheet = .x)
  ),
  all_stages
)



### 2 - Creates new column for seed code ----

# Extracts the seven digit seed code from the column school_name and inserts it in new column
# called seed_code, after the column school_name. If no such seven digit number exists, the seed_code
# value is NA

modified_data <- lapply(raw_data, function(tibble) {
  tibble %>%
    mutate(seed_code = stringr::str_extract(school_name, "\\d{7}")) %>% 
    select(hwb_la, take_part, school_name, seed_code, everything())
})



### 3 - Restricts column school_name to school name only ----

# Write function to find the last term in school_terms in school_name, remove everything after that
# E.g. "Red Primary School - City - 55" will get renamed "Red Primary School"
# E.g. "Yellow PS, Edinburgh" will get renamed "Yellow PS"
# This coding approach was chosen because there are many formats school_name can take (any many more different formats may be 
# added next year). The school_terms list is currently an exhaustive list of all the terms that a school name currently takes.
# This WILL need to be reviewed each year.

school_terms <- c("School", "Primary", "(Primary)", "PS", "Secondary", "(Secondary)", "High", "Academy", "College",
                  "Centre", "Grammar", "Service")

# Function to modify the school_name column
modify_school_name <- function(data, school_terms) {
  # Use the str_replace function to modify each 'school_name' in the data
  
  # The following regular expression captures three parts:
  # 1. (.*) - Captures everything before the last matching school term.
  # 2. (paste(school_terms, collapse = "|")) - Forms a pattern of all school terms for matching.
  # 3. .* - Captures everything after the last matching school term.
  
  # In the replacement argument:
  # "\\1" refers to the first capture group (everything before the last matching school term).
  # "\\2" refers to the second capture group (the last matching school term).
  data$school_name <- str_replace(data$school_name, paste0("(.*)(", paste(school_terms, collapse = "|"), ").*"), "\\1\\2")
  return(data)
}

# Modify the school_name column for each tibble in the list
modified_data <- lapply(modified_data, modify_school_name, school_terms)



### 4 - Validate SCN ----

# Replace invalid SCNs with "Invalid value". 
# Any value which isn't a 9 digit number or "Data not collected" will get replaced with "Invalid value".

# # Function to modify the scn column
# modify_scn_column <- function(tibble) {
#   tibble %>%
#     mutate(scn = ifelse(grepl("^\\d{9}$", scn) | scn == "Data not collected", scn, "Invalid value"))
# }
# 
# # Modify the scn column in each tibble in the list
# modified_data <- lapply(modified_data, modify_scn_column)



### 5 - Remove records where did not take part ----

# Remove records where take_part != "Yes"
modified_data <- lapply(modified_data, function(tibble) {
  tibble %>% filter(take_part == "Yes")
})

# Remove records where no questions were answered beyond the school name/seed code and SCN
# If every column except hwb_la, take_part, school_name, seed_code, scn are a combination of "No", "Data not collected" and NA
# then remove that record

# Function to filter rows based on unwanted values
filter_rows <- function(tibble) {
  tibble %>%
    filter(!if_all(
      -c(hwb_la, take_part, school_name, seed_code, scn),
      ~ . %in% c("No", "Data not collected", NA)
    ))
}

# Apply the filter function to each tibble in the list
filtered_data <- modified_data %>%
  map(~ filter_rows(.))



### 6 - Save as excel file to Merged folder ----

write_xlsx(
  filtered_data,
  file.path(raw_data_folder, year, "Merged", paste0("07_validated_school_names.xlsx"))
)



### END OF SCRIPT ###



