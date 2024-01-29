#########################################################################
# Name of file - 13_formatting_and_suppression.R
# Health and Wellbeing (HWB) Census 
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 4.2.2
#
# Description - Formats and suppresses tables for publication
#########################################################################



### 0 - Setup ----

source(here::here("code", "00_setup.R"))
source(here::here("functions", "read_raw_data.R"))
source(here::here("functions", "perform_data_suppression.R"))
source(here::here("functions", "perform_formatting.R"))
source(here::here("functions", "perform_workbook_formatting.R"))



### 1 - Read in raw data ----

# Function to read Excel files and return a list of lists of tibbles
read_excel_files <- function(directory) {
  # Get the list of Excel files in the specified directory
  excel_files <- list.files(path = directory, pattern = "\\.xlsx$", full.names = TRUE)
  
  # Initialize an empty list to store the raw data
  raw_data <- list()
  
  # Loop through each Excel file
  for (file in excel_files) {
    # Inform user about the file being read
    cat("Reading file:", file, "\n")
    
    # Extract the file name without extension
    file_name <- tools::file_path_sans_ext(basename(file))
    
    # Read all sheets from the Excel file
    sheets <- readxl::excel_sheets(file)
    
    # Initialize a list to store tibbles for each sheet
    sheet_data <- list()
    
    # Loop through each sheet in the Excel file
    for (sheet in sheets) {
      # Read the data from the current sheet
      sheet_data[[sheet]] <- readxl::read_excel(file, sheet = sheet)
    }
    
    # Add the sheet data to the raw_data list with the file name as the key
    raw_data[[file_name]] <- sheet_data
  }
  
  return(raw_data)
}

# Initialize an empty list to store all raw data
all_raw_data <- list()

# Loop through each directory
for (las in all_las) {
  # Set the file path for the current directory
  file_path <- here("output", year, las, "Output")
  
  # Inform user about the directory being processed
  cat("Processing directory:", file_path, "\n")
  
  # Read data for the current directory
  raw_data <- read_excel_files(file_path)
  
  # Append the results to the list
  all_raw_data[[las]] <- raw_data
}

# Read data from the "National" folder and add to all_raw_data list
national_file_path <- here("output", year, "National", "Output")
cat("Processing directory:", national_file_path, "\n")
national_raw_data <- read_excel_files(national_file_path)
all_raw_data[["National"]] <- national_raw_data



### 2 - Read in column names metadata ----

# This is used to replace values in the 'Survey question' column when formatting
# Read in metadata headers for each stage and store as a list of data frames
metadata_headers <- 
  map(
    set_names(all_stages),
    ~ read_xlsx(here(
      "metadata", 
      year,
      paste0("hwb_metadata_", tolower(.x), "_column_names.xlsx")
    ))
  ) 

# Restrict each tibble to only the columns "code" and "question_header2"
metadata_headers <- lapply(metadata_headers, function(tibble) {
  select(tibble, code, question_header2) %>%
    mutate(
      question_header2 = str_remove(question_header2, paste0(q_pattern, "\\s"))
    )
})

# Convert list of dataframes into one dataframe using bind_rows, and remove duplicates
metadata_headers <- bind_rows(metadata_headers) %>% 
  distinct()



### 3 - Join dataframes for each local authority together ----

# Function to add 'Survey topic' column as the first column in each tibble
add_survey_topic_column <- function(data, topic_name) {
  lapply(data, function(tbl) {
    tbl <- cbind(`Survey topic` = rep(topic_name, each = nrow(tbl)), tbl)
    tbl
  })
}

# Loop through each top-level list in all_raw_data
for (region_name in names(all_raw_data)) {
  region <- all_raw_data[[region_name]]
  
  # Loop through sublists within each top-level list
  for (sublist_name in names(region)) {
    sublist <- region[[sublist_name]]
    
    # Check if sublist is a list
    if (is.list(sublist)) {
      # Add 'Survey topic' column to each tibble in the sublist
      all_raw_data[[region_name]][[sublist_name]] <- add_survey_topic_column(sublist, sublist_name)
    }
  }
}


# Function to row bind tibbles with the same name
bind_same_name <- function(lst) {
  # Get unique names of tibbles
  tibble_names <- names(lst[[1]])

  # Row bind tibbles with the same name, storing the result in a named list
  tibble_list <- map(tibble_names, ~ map_dfr(lst, .x))

  # Name the list elements after the original tibble names
  names(tibble_list) <- tibble_names

  # Return the row binded tibbles in a named list
  return(tibble_list)
}

# Apply bind_same_name function to each sub-list in all_raw_data
combined_data <- map(all_raw_data, bind_same_name)


# Function to clean the 'Survey topic' values from "2022_attitudes_to_school_and_aspirations" to "Attitudes to School and Aspirations"
clean_survey_topic <- function(topic_name) {
  # Remove numeric characters and first "_", replace remaining "_" with " ", and capitalize the first letter
  cleaned_topic <- gsub("^\\d+_", "", topic_name)  # Remove leading numeric characters followed by "_"
  cleaned_topic <- gsub("_", " ", cleaned_topic)  # Replace remaining "_" with " "
  cleaned_topic <- tools::toTitleCase(cleaned_topic)  # Capitalize the first letter of each word

  # Capitalize specific strings
  cleaned_topic <- gsub("\\bsdq\\b", "SDQ", cleaned_topic, ignore.case = TRUE)
  cleaned_topic <- gsub("\\bwemwbs\\b", "WEMWBS", cleaned_topic, ignore.case = TRUE)

  cleaned_topic
}

# Loop through each top-level list in combined_data
for (region_name in names(combined_data)) {
  region <- combined_data[[region_name]]

  # Loop through sublists within each top-level list
  for (sublist_name in names(region)) {
    sublist <- region[[sublist_name]]

    # Check if sublist is a list
    if (is.list(sublist)) {
      # Clean up the 'Survey topic' column values
      combined_data[[region_name]][[sublist_name]][['Survey topic']] <- clean_survey_topic(combined_data[[region_name]][[sublist_name]][['Survey topic']])
    }
  }
}

# original_data <- combined_data$Angus

a <- combined_data$National$stage
b <- combined_data$Shetland$stage


# If rows are missing from a tibble, add them in
# This happens for smaller local authorities, e.g. Shetland, which may have no-one in that local authority choosing a certain response option

# Extract the National list of dataframes
national_list <- list(stage = combined_data$National$stage,
                      sex = combined_data$National$sex,
                      simd = combined_data$National$simd,
                      urbrur6 = combined_data$National$urbrur6,
                      ethnic_group = combined_data$National$ethnic_group,
                      care_for_someone = combined_data$National$care_for_someone,
                      long_term_condition = combined_data$National$long_term_condition
)

# Loop over each dataframe in the National list
for (df_name in names(national_list)) {
  # Get the dataframe from National list
  national_df <- national_list[[df_name]]
  
  # Loop over each row in the dataframe
  for (i in 1:nrow(national_df)) {
    # Extract the first three columns
    row_key <- national_df[i, 1:3]
    
    # Check if the row exists in the respective dataframe
    if (!(df_name %in% names(combined_data)) || 
        is.null(combined_data[[df_name]])) {
      combined_data[[df_name]] <- data.frame(matrix(0, ncol = ncol(national_df), nrow = 0))
      colnames(combined_data[[df_name]]) <- colnames(national_df)
    }
    
    matching_row <- combined_data[[df_name]][combined_data[[df_name]][, 1] == row_key[1] & 
                                               combined_data[[df_name]][, 2] == row_key[2] & 
                                               combined_data[[df_name]][, 3] == row_key[3], ]
    
    # If no matching row is found, add a new row with 0s for other columns
    if (nrow(matching_row) == 0) {
      new_row <- c(row_key, rep(0, ncol(national_df) - 3))
      combined_data[[df_name]] <- rbind(combined_data[[df_name]], new_row)
    }
  }
}






### 4 - Perform data suppression ---

## From here the Response column is getting deleted. Why???

all_data_suppressed <- lapply(combined_data, perform_data_suppression)



### 5 - Format data as dataframes ----

# all_data_suppressed <- perform_formatting(all_data_suppressed)

# formatted_data <- list()
# 
# for (la in all_las) {
#   formatted_data[[la]] <- lapply(all_data_suppressed[[la]], perform_formatting)
# }

formatted_data <- lapply(all_data_suppressed, perform_formatting)



### 5 - Build workbooks ----

# Create a list of workbooks dynamically for each la and topic
workbook_list <- lapply(names(formatted_data), function(la) {
  subcategories <- names(formatted_data[[la]])
  workbooks <- lapply(subcategories, function(topic) {
    buildWorkbook(formatted_data[[la]][[topic]], gridLines = FALSE)
  })
  names(workbooks) <- subcategories
  return(workbooks)
})

names(workbook_list) <- names(formatted_data)



### 6 - Apply workbook formatting ----

formatted_workbook_list <- list()

for (county_name in names(workbook_list)) {
  county <- workbook_list[[county_name]]
  
  for (sheet_name in names(county)) {
    workbook <- workbook_list[[county_name]][[sheet_name]]
    formatted_data_sheet <- formatted_data[[county_name]][[sheet_name]]
    
    result <- perform_workbook_formatting(workbook, formatted_data_sheet)
    
    formatted_workbook_list[[paste0(county_name, "_", sheet_name)]] <- result
  }
}



### 7 - Save outputs as an excel file to Merged folder ----

# Saves the workbook as an excel sheet 
saveWorkbook(wb,here("output", year, "Shetland", "Suppressed and formatted", paste0(year, "_attitudes_to_school_and_aspirations_formatted.xlsx")), overwrite = TRUE)



### END OF SCRIPT ###


# To do
# re-order columns. prob easiest to set prefer not to say and not known last rather than list them all
# add in columns if blank e.g. simd 1 in shetland

