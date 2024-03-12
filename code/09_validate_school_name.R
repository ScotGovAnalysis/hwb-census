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

# Read all sheets into a list of tibbles 
raw_data <- 
  map(
    survey_names,
    ~ read_xlsx(file.path(raw_data_folder, year, "Merged", "06_validated_routing.xlsx"), sheet = .x)
  )



### 2 - Creates new column for seed code for HWB data ----

# Extracts the seven digit seed code from the column school_name and inserts it in new column
# called seed_code, after the column school_name. If no such seven digit number exists, the seed_code
# value is NA
modified_data <- lapply(raw_data, function(tibble) {
  if ("school_name" %in% colnames(tibble)) {
    tibble %>%
      mutate(seed_code = stringr::str_extract(school_name, "\\d{7}")) %>% 
      select(hwb_la, take_part, school_name, seed_code, everything())
  } else {
    tibble  # If school_name doesn't exist, return the original tibble
  }
})



### 3 - Restricts column school_name to school name only for HWB data ----

# Write function to find the last term in school_terms in school_name, remove everything after that
# E.g. "Red Primary School - City - 55" will get renamed "Red Primary School"
# E.g. "Yellow PS, Edinburgh" will get renamed "Yellow PS"
# This coding approach was chosen because there are many formats school_name can take (any many more different formats may be 
# added next year). The school_terms list is currently an exhaustive list of all the terms that a school name currently takes.
# This WILL need to be reviewed each year.

school_terms <- c("School", "Primary", "PS", "Secondary", "High", "Academy", "College",
                  "Centre", "Grammar", "Service")

# Function to modify the school_name column
modify_school_name <- function(data, school_terms) {
  if ("school_name" %in% colnames(data)) {
  # Use the str_replace function to modify each 'school_name' in the data
  
  # The following regular expression captures three parts:
  # 1. (.*) - Captures everything before the last matching school term.
  # 2. (paste(school_terms, collapse = "|")) - Forms a pattern of all school terms for matching.
  # 3. .* - Captures everything after the last matching school term.
  
  # In the replacement argument:
  # "\\1" refers to the first capture group (everything before the last matching school term).
  # "\\2" refers to the second capture group (the last matching school term).
  data$school_name <- str_replace(data$school_name, paste0("(.*)(\\(?(", paste(school_terms, collapse = "|"), ")\\/?).*"), "\\1\\2")
  }
  return(data)
}

# Modify the school_name column for each tibble in the list
modified_data <- lapply(modified_data, modify_school_name, school_terms)



### 4 - Remove records where pupil did not consent to take part ----

# Remove records where take_part != "Yes" 
modified_data <- lapply(modified_data, function(tibble) {
  tibble %>% filter(take_part == "Yes")
})


# Remove records where no questions were answered beyond the school name/seed code and SCN for HWB data
# If every column except hwb_la, take_part, school_name, seed_code, scn are a combination of "No", "Data not collected" and NA
# then remove that record

# Function to filter rows based on unwanted values for HWB data
filter_rows <- function(tibble) {
  tibble %>%
    filter(!if_all(
      -c(hwb_la, take_part, school_name, seed_code, scn),
      ~ . %in% c("No", "Data not collected", NA)
    ))
}

# Apply the filter function to each HWB tibble in the list (exclude substance use)
filtered_data <- modified_data %>%
  map_if(names(modified_data) != "S4_SU", filter_rows, .else = ~ .x)


# Remove rows where all columns except "hwb_la", "take_part", "sex" and "simd_decile" are a combination of "No", "Data not collected" and NA for substance use data
filtered_data$S4_SU <- filtered_data$S4_SU %>%
    filter(!if_all(
      -c(hwb_la, take_part, sex, simd_decile),
      ~ . %in% c("No", "Data not collected", NA)
    ))



# Split filtered_data into HWB_data and substance_use_data. This is because we have now finished cleaning substance use data and it is ready for analysis, whereas 
# HWB requires further cleaning.
substance_use_data <- filtered_data$S4_SU

hwb_data <- filtered_data[!names(filtered_data) %in% "S4_SU"]



### 5 - Save as excel file to Merged folder ----

# Save excel file to output folder for HWB
write_xlsx(
  hwb_data,
  file.path(raw_data_folder, year, "Merged", paste0("07_validated_school_names_hwb.xlsx"))
)

# Save excel file to output folder for substance use
write_xlsx(
  substance_use_data,
  file.path(raw_data_folder, year, "Merged", paste0("07_validated_school_names_su.xlsx"))
)


### END OF SCRIPT ###
