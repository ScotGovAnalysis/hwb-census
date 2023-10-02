#########################################################################
# Name of file - 03_rename_headers.R
# Health and Wellbeing (HWB) Census 
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 4.2.2
#
# Description - Renames headers of sumbitted data files, after headers have been validated
#########################################################################


### 0 - Setup ----

source(here::here("code", "00_setup.R"))
source(here::here("functions", "read_raw_data.R"))


### 1 - Expected headers ----

# Read in expected headers for each stage and store as a list of data frames
exp_headers <- 
  map(
    set_names(all_stages),
    ~ read_xlsx(here(
      "metadata", 
      year,
      paste0("hwb_metadata_", tolower(.x), "_column_names.xlsx")
    ))
  ) 

# Clean and restructure expected headers data
# NOTE - slightly different than in scripts 01 and 02
exp_headers <- exp_headers |>
  map(\(x)   
      x |>
        # Recode character NAs as system NAs
        mutate(across(where(is.character), ~ na_if(., "NA"))) |>
        # Remove la_code header (this is not included in raw data submissions)
        filter(!code %in% c("la_code", "seed_code")) |>
        # Keep raw header columns only
        select(code, question_header1, question_header2) |>
        # Clean strings
        mutate(across(everything(), clean_strings)) |>
        # Use merged headers only
        select(code, h = question_header2) |>
        # Extract question number
        mutate(
          q  = str_extract(h, q_pattern),
          h = str_remove(h, paste0(q_pattern, "\\s"))
        ) |>
        # Add count of duplicate headers
        group_by(h) |>
        mutate(n = n()) |>
        ungroup()
  )

exp_headers <-
  map(
    set_names(all_stages),
    ~ exp_headers[[.x]] %>%
        mutate(h = make_clean_names(h)))



### 2 - Check headers of raw data ----

# Read in each dataframe from 02_validated_headers for each LA and stage
raw_data <- 
  map(all_las, function(las) {
    map(all_stages, function(stage) {
      read_raw_data(year, las, stage, "02_validated_headers")
    })
  })


# Rename elements within raw_data to respective local authority
names(raw_data) <- c(all_las)


# Create function to rename each tibble within list of LA's to correct stage
rename_tibbles <- function(lst) {
  names(lst) <- all_stages
  return(lst)
}

raw_data <- lapply(raw_data, rename_tibbles)


# Apply clean_strings to raw_data to header row only
raw_data <- raw_data %>%
  map(function(sublist) {
    sublist %>%
      map_if(is.data.frame, ~ {
        header <- names(.x)
        body <- .x
        names(body) <- clean_strings(header)
        body
      })
  })


# Define a function to adjust the header row for each tibble
# For tibbles with two header rows, they should be merged so the header row matches exp_headers
adjust_header <- function(tbl){
  header_row <- colnames(tbl)
  
  # Check if any header cell matches the pattern "...NUMBER"
  if(any(grepl("^\\.\\.\\.[0-9]+$", header_row))){
    for (i in seq_along(header_row)) {
      if(!is.na(tbl[1, i])) {
         header_row[i] <- tbl[1, i]
      }
    }
    tbl <- tbl[-1, ]
  }
  
  colnames(tbl) <- header_row
  
  return(tbl)
}

# Apply adjust_header function to every tibble
adjusted_headers <- map(raw_data, ~ map(.x, ~ adjust_header(.x)))


# Define a function to rename the columns of each tibble
rename_columns <- function(tibble) {
  tibble %>%
        rename_with(~ make_clean_names(str_remove(.x, paste0(q_pattern, "\\s"))))
    }

# Apply rename_columns function to each tibble
renamed_headers <- lapply(adjusted_headers, function(sublist) {
  lapply(sublist, rename_columns)
})





### 3 - Check headers of raw data have all been renamed, flag those which haven't ----

# Check all headers have been renamed
# Create an empty list to store the results
missing_headers_list <- list()

# Iterate through each list of tibbles in renamed_headers
for (i in seq_along(renamed_headers)) {
  # Get the name of the current list
  list_name <- names(renamed_headers)[i]
  
  # Create a list to store the missing headers for the current list
  list_missing_headers <- list()
  
  # Iterate through each tibble in the current list
  for (j in seq_along(renamed_headers[[i]])) {
    # Get the name of the current tibble
    tibble_name <- names(renamed_headers[[i]])[j]
    
    # Extract the current tibble from renamed_headers
    current_renamed_tibble <- renamed_headers[[i]][[j]]
    
    # Find the corresponding tibble in exp_headers by name
    current_exp_tibble <- exp_headers[[tibble_name]]
    
    # Get the values in the "h" column of the current exp_tibble
    exp_h_values <- current_exp_tibble$h
    
    # Find headers in current_renamed_tibble that are not in exp_h_values
    missing_headers <- setdiff(colnames(current_renamed_tibble), exp_h_values)
    
    # Store the missing headers for the current tibble
    list_missing_headers[[tibble_name]] <- missing_headers
  }
  
  # Store the missing headers for the current list
  missing_headers_list[[list_name]] <- list_missing_headers
}


# Print only the tibbles with differing headers
for (list_name in names(missing_headers_list)) {
  list_missing_headers <- missing_headers_list[[list_name]]
  for (tibble_name in names(list_missing_headers)) {
    missing_headers <- list_missing_headers[[tibble_name]]
    if (!identical(missing_headers, character(0))) {
      cat("List:", list_name, "| Tibble:", tibble_name, "| Missing Headers:", toString(missing_headers), "\n")
    }
  }
}



### 4 - Save renamed headers ----

# Save excel file to output folder
# Loop through each LA and list of stage tibbles
for (folder_name in all_las) {
  # Get the list of tibbles for the current LA
  tibble_list <- renamed_headers[[folder_name]]
  
  # Loop through each tibble in the list
  for (tibble_name in names(tibble_list)) {
    # Create the file name for the Excel file
    file_name <- file.path("//s0196a/ADM-Education-NIF Analysis/Health and Wellbeing Survey/R/RAP Project/raw_data", year, folder_name, "03_renamed_headers", paste0(folder_name, "_", tibble_name, ".xlsx"))
    
    # Get the tibble
    tibble_data <- tibble_list[[tibble_name]]
    
    # Save the tibble as an Excel file
    write_xlsx(tibble_data, file_name)
  }
}

  
  





