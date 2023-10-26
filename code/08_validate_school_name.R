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

# Removes everything after the occurrence of the string "School"
modified_data <- lapply(modified_data, function(tibble) {
  tibble$school_name <- sub("School.*", "School", tibble$school_name)
  return(tibble)
})

# Removes everything after the occurrence of the string "(Primary)", only if the string does not contain "School"
modified_data <- lapply(modified_data, function(df) {
  df$school_name <- ifelse(grepl("School", df$school_name), df$school_name, sub("\\(Primary\\).*", "(Primary)", df$school_name))
  return(df)
})

# Removes everything after the occurrence of the string "(Secondary)", only if the string does not contain "School"
modified_data <- lapply(modified_data, function(df) {
  df$school_name <- ifelse(grepl("School", df$school_name), df$school_name, sub("\\(Secondary\\).*", "(Secondary)", df$school_name))
  return(df)
})

# Removes everything after the occurrence of the string "Primary", only if the string does not contain "School"
modified_data <- lapply(modified_data, function(tibble) {
  tibble$school_name <- ifelse(grepl("School", tibble$school_name), tibble$school_name, sub("Primary.*", "Primary", tibble$school_name))
  return(tibble)
})

# Removes everything after the occurrence of the string "PS", only if the string does not contain "School" and "Primary"
modified_data <- lapply(modified_data, function(tibble) {
  tibble$school_name <- ifelse(grepl("School|Primary", tibble$school_name), tibble$school_name, sub("PS.*", "PS", tibble$school_name))
  return(tibble)
})

# Removes everything after the occurrence of the string "Secondary", only if the string does not contain "School" and "Primary" and "PS"
modified_data <- lapply(modified_data, function(tibble) {
  tibble$school_name <- ifelse(grepl("School|Primary|PS", tibble$school_name), tibble$school_name, sub("Secondary.*", "Secondary", tibble$school_name))
  return(tibble)
})

# Removes everything after the occurrence of the string "High", only if the string does not contain "School" and "Primary" and "PS" and "Secondary"
modified_data <- lapply(modified_data, function(tibble) {
  tibble$school_name <- ifelse(grepl("School|Primary|PS|Secondary", tibble$school_name), tibble$school_name, sub("High.*", "High", tibble$school_name))
  return(tibble)
})

# Removes everything after the occurrence of the string "Academy", only if the string does not contain "School" and "Primary" and "PS" and "Secondary" and "High"
modified_data <- lapply(modified_data, function(tibble) {
  tibble$school_name <- ifelse(grepl("School|Primary|PS|Secondary|High", tibble$school_name), tibble$school_name, sub("Academy.*", "Academy", tibble$school_name))
  return(tibble)
})

# Removes everything after the occurrence of the string "College", only if the string does not contain "School" and "Primary" and "PS" and "Secondary" and "High" and "Academy"
modified_data <- lapply(modified_data, function(tibble) {
  tibble$school_name <- ifelse(grepl("School|Primary|PS|Secondary|High|Academy", tibble$school_name), tibble$school_name, sub("College.*", "College", tibble$school_name))
  return(tibble)
})

# Removes everything after the occurrence of the string "Centre", only if the string does not contain "School" and "Primary" and "PS" and "Secondary" and "High" and "Academy" and "College"
modified_data <- lapply(modified_data, function(tibble) {
  tibble$school_name <- ifelse(grepl("School|Primary|PS|Secondary|High|Academy|College", tibble$school_name), tibble$school_name, sub("Centre.*", "Centre", tibble$school_name))
  return(tibble)
})

# Removes everything after the occurrence of the string "Grammar", only if the string does not contain "School" and "Primary" and "PS" and "Secondary" and "High" and "Academy" and "College" and "Centre"
modified_data <- lapply(modified_data, function(tibble) {
  tibble$school_name <- ifelse(grepl("School|Primary|PS|Secondary|High|Academy|College|Centre", tibble$school_name), tibble$school_name, sub("Grammar.*", "Grammar", tibble$school_name))
  return(tibble)
})

# Removes everything after the occurrence of the string "Service", only if the string does not contain "School" and "Primary" and "PS" and "Secondary" and "High" and "Academy" and "College" and "Centre" and "Grammar"
modified_data <- lapply(modified_data, function(tibble) {
  tibble$school_name <- ifelse(grepl("School|Primary|PS|Secondary|High|Academy|College|Centre|Grammar", tibble$school_name), tibble$school_name, sub("Service.*", "Service", tibble$school_name))
  return(tibble)
})



### 4 - Validate SCN ----

# Replace invalid SCNs with "Invalid value". 
# Any value which isn't a 9 digit number or "Data not collected" will get replaced with "Invalid value".

# Function to modify the scn column
modify_scn_column <- function(tibble) {
  tibble %>%
    mutate(scn = ifelse(grepl("^\\d{9}$", scn) | scn == "Data not collected", scn, "Invalid value"))
}

# Modify the scn column in each tibble in the list
modified_data <- lapply(modified_data, modify_scn_column)



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










