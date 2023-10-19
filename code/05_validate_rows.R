#########################################################################
# Name of file - 05_validate_headers.R
# Health and Wellbeing (HWB) Census 
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 4.2.2
#
# Description - Validates rows of merged data files and creates an
# excel file summarising issues by Local Authority and Stage
#########################################################################


### 0 - Setup ----

source(here::here("code", "00_setup.R"))



### 1 - Expected rows ----

# Read in expected rows for each stage and store as a list of data frames
exp_rows <- 
  map(
    set_names(all_stages),
    ~ read_xlsx(here(
      "metadata", 
      year,
      paste0("hwb_metadata_", tolower(.x), "_row_names.xlsx")
    ))
  ) 

# Replace all non-breaking spaces (NBSP with regular spaces)
exp_rows <- map(exp_rows, ~ .x %>% 
                  mutate_all(~ gsub("\u00A0", " ", .)))



### 2 - Read in data ----

# Read in merged data for each stage and store as a list of data frames

# Define the path to your Excel file and the sheet names
file_path <- file.path(raw_data_folder, year, "Merged", "04_merged_data.xlsx")

# Read in all sheets into a list of tibbles with names
act_rows <- set_names(
  map(
    all_stages,
    ~ read_xlsx(file_path, sheet = .x)
  ),
  all_stages
)

# Replace all non-breaking spaces (NBSP with regular spaces)
act_rows <- map(act_rows, ~ .x %>% 
                  mutate_all(~ gsub("\u00A0", " ", .)))


# Remove columns "scn" and "school_name"
# Define a function to remove columns from a tibble
remove_columns <- function(tibble) {
  tibble %>% select(-school_name, -scn)
}

# Use lapply to apply the function to each tibble in the list
act_rows <- lapply(act_rows, remove_columns)



### 3 - Replace all "-" values in act_rows with "NA"

# Function to replace "-" with "NA" in a tibble
replace_dash_with_na <- function(tib) {
  tib %>% mutate_all(~ ifelse(. == "-", "NA", .))
}

# Use map to apply the function to each tibble in the list
act_rows <- map(act_rows, replace_dash_with_na)



### 4 - Compare act_rows to exp_rows ----

# This code loops through the tibbles in act_rows, finds the corresponding tibble 
# in exp_rows by name, and then calculates the unique values for each column. 
# The resulting tibbles are stored in the unexp_rows list with the same names.

# Initialize an empty list to store the result
unexp_rows <- list()

# Loop through the tibbles in act_rows
for (i in 1:length(act_rows)) {
  # Find the corresponding tibble in exp_rows by name
  matching_exp_tibble <- exp_rows[[i]]
  
  # Check if a match was found
  if (!is.null(matching_exp_tibble)) {
    # Initialize a list to store unique values for each column
    unique_columns <- list()
    
    # Loop through the columns of the tibble
    for (col_name in colnames(matching_exp_tibble)) {
      # Calculate unique values that are in act_rows but not in exp_rows for each column
      unique_col <- setdiff(act_rows[[i]][[col_name]], matching_exp_tibble[[col_name]])
      unique_columns[[col_name]] <- unique_col
    }
    
    # Create a new tibble-like list (matrix) with unique columns
    # Match the name of the resulting tibble with the exp_rows tibble name
    unexp_rows[[names(exp_rows)[i]]] <- unique_columns
  }
  else {
    # If no matching tibble was found in exp_rows, create an empty list
    unexp_rows[[i]] <- list()
  }
}

# Define a custom function to filter out zero-length vectors
# (i.e. questions which didn't have any unexpected response options)
remove_zero_length <- function(lst) {
  Filter(function(vec) length(vec) > 0, lst)
}

# Apply the custom function to remove zero-length vectors
unexp_rows_filtered <- lapply(unexp_rows, remove_zero_length)


# Function to convert a list of character vectors into a dataframe
list_to_dataframe <- function(lst) {
  max_length <- max(lengths(lst))
  filled_lst <- lapply(lst, function(vec) {
    if (length(vec) < max_length) {
      c(vec, rep(NA, max_length - length(vec)))
    } else {
      vec
    }
  })
  data.frame(do.call(cbind, filled_lst))
}

# Convert the list of lists into a list of dataframes
list_of_dataframes <- lapply(unexp_rows_filtered, list_to_dataframe)



### 5 - Save row validation summary to output folder

# Save excel file to output folder
write_xlsx(
  list_of_dataframes, 
  here("output", year, paste0(year, "_row-validation.xlsx"))
)



### END OF SCRIPT ###

