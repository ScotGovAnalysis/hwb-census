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



### 4 - Add missing rows to dataframes ----

# Define the process_data function
process_data <- function(national, la) {
  # Identify rows in national that are not present in la
  rows_to_add <- anti_join(national, la, by = c("Survey topic", "Survey question", "Response"))
  
  # Check if there are rows to add
  if (nrow(rows_to_add) > 0) {
    # Add "0" to other columns if they exist
    for (col in setdiff(names(la), c("Survey topic", "Survey question", "Response"))) {
      if (col %in% names(rows_to_add)) {
        rows_to_add[[col]] <- "0"
      }
    }
    
    # Append the rows to la
    la <- bind_rows(la, rows_to_add)
  }
  
  # Reorder la based on the row indices of national
  la_ordered <- la[match(
    paste(national$`Survey topic`, national$`Survey question`, national$Response),
    paste(la$`Survey topic`, la$`Survey question`, la$Response)
  ), ]
  
  # Reorder columns to match national
  common_cols <- intersect(names(national), names(la_ordered))
  la_ordered <- la_ordered[, common_cols]
  
  return(la_ordered)
}


# Create rows_added_data to store the processed data
rows_added_data <- list()

# Include National dataframe in rows_added_data
rows_added_data[["National"]] <- combined_data[["National"]]

# Loop over each local authority (la) in all_las
for (la in all_las) {
  # Create a sublist in rows_added_data for each la
  rows_added_data[[la]] <- list()
  
  # Loop over each dataframe in the current la
  for (i in seq_along(combined_data[[la]])) {
    name_la <- names(combined_data[[la]])[[i]]
    
    # Apply process_data to compare with the respective dataframe in National
    processed_df <- process_data(combined_data[["National"]][[name_la]], combined_data[[la]][[i]])
    
    # Store the processed dataframe in the sublist of rows_added_data
    # Preserve the name of the dataframe
    name <- names(combined_data[[la]])[i]
    rows_added_data[[la]][[name]] <- processed_df
  }
}








# Define the process_data function
process_data2 <- function(national, la) {
  # Identify rows in national that are not present in la
  rows_to_add <- anti_join(national, la, by = c("Survey topic", "Survey question", "Response"))
  
  # Check if there are rows to add
  if (nrow(rows_to_add) > 0) {
    # Add "0" to other columns if they exist
    for (col in setdiff(names(la), c("Survey topic", "Survey question", "Response"))) {
      if (col %in% names(rows_to_add)) {
        rows_to_add[[col]] <- "0"
      }
    }
    
    # Append the rows to la
    la <- bind_rows(la, rows_to_add)
  }
  
  # Reorder la based on the row indices of national
  la_ordered <- la[match(
    paste(national$`Survey topic`, national$`Survey question`, national$Response),
    paste(la$`Survey topic`, la$`Survey question`, la$Response)
  ), ]
  
  # Reorder columns to match national
  common_cols <- intersect(names(national), names(la_ordered))
  la_ordered <- la_ordered[, common_cols]
  
  return(la_ordered)
}


# Create rows_added_data2 to store the processed data
rows_added_data2 <- list()

# Include National dataframe in rows_added_data2
rows_added_data2[["National"]] <- rows_added_data[["National"]]

# Loop over each local authority (la) in all_las
for (i1 in seq_along(rows_added_data))  {
  # Get the name of the current list of data frames
  name_i1 <- names(rows_added_data)[i1]
  # Create a sublist in rows_added_data2 for each i1
  rows_added_data2[[name_i1]] <- list()
  
  # Loop over each dataframe in the current i1
  for (i2 in seq_along(rows_added_data[[i1]])) {
    
    # Apply process_data2 to compare with the respective dataframe in National
    processed_df <- process_data2(rows_added_data[["National"]][["stage"]], rows_added_data[[i1]][[i2]])
    
    # Store the processed dataframe in the sublist of rows_added_data2
    # Preserve the name of the dataframe
    name <- names(rows_added_data[[i1]])[i2]
    rows_added_data2[[i1]][[name]] <- processed_df
    
  }

}


test <- combined_data$Shetland$urbrur6







##### Need to fix the different number of rows in different dataframes
# Caused by carers analysis not in lt_illness, caring and urbrur6

national <- rows_added_data$National$stage
la <- rows_added_data$Shetland$urbrur6



# Define the process_data function
process_data2 <- function(national, la) {
  # Identify rows in national that are not present in la
  rows_to_add <- anti_join(national, la, by = c("Survey topic", "Survey question", "Response"))
  
  # Check if there are rows to add
  if (nrow(rows_to_add) > 0) {
    # Add "0" to other columns if they exist
    for (col in setdiff(names(la), c("Survey topic", "Survey question", "Response"))) {
      if (col %in% names(rows_to_add)) {
        rows_to_add[[col]] <- "0"
      }
    }
    
    # Append the rows to la
    la <- bind_rows(la, rows_to_add)
  }
  
  # Reorder la based on the row indices of national
  la_ordered <- la[match(
    paste(national$`Survey topic`, national$`Survey question`, national$Response),
    paste(la$`Survey topic`, la$`Survey question`, la$Response)
  ), ]
  
  # Reorder columns to match national
  common_cols <- intersect(names(national), names(la_ordered))
  la_ordered <- la_ordered[, common_cols]
  
  return(la_ordered)
}


# Create rows_added_data2 to store the processed data
rows_added_data2 <- list()

# Include National dataframe in rows_added_data2
rows_added_data2[["National"]] <- rows_added_data[["National"]]

# Loop over each local authority (la) in all_las
for (i1 in seq_along(rows_added_data))  {
  # Get the name of the current list of data frames
  name_i1 <- names(rows_added_data)[i1]
  # Create a sublist in rows_added_data2 for each i1
  rows_added_data2[[name_i1]] <- list()
  
  # Loop over each dataframe in the current i1
  for (i2 in seq_along(rows_added_data[[i1]])) {
    
    # Apply process_data2 to compare with the respective dataframe in National
    processed_df <- process_data2(rows_added_data[["National"]][["stage"]], rows_added_data[[i1]][[i2]])
    
    # Store the processed dataframe in the sublist of rows_added_data2
    # Preserve the name of the dataframe
    name <- names(rows_added_data[[i1]])[i2]
    rows_added_data2[[i1]][[name]] <- processed_df
    
  }
  
}













### 5 - Perform data suppression ---

all_data_suppressed <- lapply(combined_data, perform_data_suppression)



### 6 - Format data as dataframes ----

# all_data_suppressed <- perform_formatting(all_data_suppressed)

# formatted_data <- list()
# 
# for (la in all_las) {
#   formatted_data[[la]] <- lapply(all_data_suppressed[[la]], perform_formatting)
# }

formatted_data <- lapply(all_data_suppressed, perform_formatting)



### 7 - Build workbooks ----

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



# works for this::
# formatted_data <- formatted_data$Angus
# wb <- buildWorkbook(formatted_data, gridLines = FALSE)
# 
# la <- "Angus"


### 8 - Apply workbook formatting ----

formatted_workbook_list <- list()
# 
# for (la_name in names(workbook_list)) {
#  # la <- workbook_list[[la_name]]
#   
#   # for (sheet_name in names(la)) {
#     workbook <- buildWorkbook(workbook_list[[la_name]], gridLines = FALSE)# [[sheet_name]]
#     data <- formatted_data[[la_name]]# [[sheet_name]]
#     
#     result <- perform_workbook_formatting(workbook, formatted_data_sheet)
#     
#     formatted_workbook_list[[paste0(la_name)]] <- result
#   # }
# }

# for (la in names(formatted_data)){
#   data <- formatted_data[[la]]
#   wb <- buildWorkbook(data, gridLines = FALSE)
#   result <- perform_workbook_formatting(wb, data)
#   formatted_workbook_list[[paste0(la)]] <- result
#   
# }

# Loop through each element in formatted_data
for (la in names(formatted_data)) {
  cli_inform(paste("Processing:", la))
  
  # Extract data for the current element
  data <- formatted_data[[la]]
  
  # Build workbook
  wb <- buildWorkbook(data, gridLines = FALSE)
  
  # Perform workbook formatting
  result <- perform_workbook_formatting(wb, data)
  
  # Store the formatted workbook in the list
  formatted_workbook_list[[paste0(la)]] <- result
}


 




wb <- formatted_workbook_list$Shetland



### 9 - Save outputs as an excel file to Merged folder ----

# Saves the workbook as an excel sheet 
saveWorkbook(wb,here("output", year, "Shetland", "Suppressed and formatted", paste0(year, "_attitudes_to_school_and_aspirations_formatted2.xlsx")), overwrite = TRUE)



### END OF SCRIPT ###


# To do
# re-order columns. prob easiest to set prefer not to say and not known last rather than list them all
# add in columns if blank e.g. simd 1 in shetland

