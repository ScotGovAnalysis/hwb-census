#########################################################################
# Name of file - 03_rename_headers.R
# Health and Wellbeing (HWB) Census 
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 4.2.2
#
# Description - Renames headers of submitted data files, after headers have been validated
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

# Apply make_clean_names to column h (strip out punctuation and replace " " with "_")
exp_headers <-
  map(
    set_names(all_stages),
    ~ exp_headers[[.x]] %>%
        mutate(h = make_clean_names(h)))

# Read in expected headers for substance use
exp_headers_su <- 
  read_xlsx(here(
    "metadata", 
    year,
    paste0("hwb_metadata_s4_su_column_names.xlsx")
  ))

# Clean and restructure expected headers for substance use data
# NOTE - slightly different than in scripts 01 and 02
exp_headers_su <- exp_headers_su |>
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

# Apply make_clean_names to column h (strip out punctuation and replace " " with "_")
exp_headers_su <- exp_headers_su %>%
      mutate(h = make_clean_names(h))



### 2 - Check headers of raw data ----

# Read raw data for each combination of local authority and stage
# Store the data in a nested list structure
raw_data <- setNames(
  lapply(
    map(all_las, function(las) {
      map(all_stages, function(stage) {
        # Display a message indicating which file is currently being read
        cli_inform(sprintf("Reading data for local authority '%s' and stage '%s'", las, stage))
        
        # Read raw data for the current combination of local authority and stage
        read_raw_data(year, las, stage, "02_validated_headers")
      }) %>% 
        # Rename the tibbles within the inner list to the corresponding stage names
        setNames(all_stages)
    }),
    # Retain the same structure of the outer list
    identity
  ),
  # Set the names of the outer list to the local authority names
  all_las
)


# Read raw data for each local authority for substance use
# Store the data in a list 
raw_data_su <- setNames(
  map(all_las, function(las) {
    # Display a message indicating which file is currently being read
    cli_inform(sprintf("Reading substance use data for local authority '%s'", las))
    
    # Read raw data for the current local authority
    read_raw_data(year, las, "S4", "02_validated_headers_substance_use")
  }),
  # Set the names of tibble to the local authority names
  all_las
)


# Define a function to adjust the header row for each tibble
# For tibbles with two header rows, they should be merged into one so the header row matches exp_headers
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

# Apply adjust_header function to every tibble in raw_data
adjusted_headers <- map(raw_data, ~ map(.x, ~ adjust_header(.x)))

# Apply adjust_header function to every tibble in raw_data_su
adjusted_headers_su <- map(raw_data_su, adjust_header)

# Apply clean_strings to adjusted_headers to header row only
adjusted_headers <- adjusted_headers %>%
  map(function(sublist) {
    sublist %>%
      map_if(is.data.frame, ~ {
        header <- names(.x)
        body <- .x
        names(body) <- clean_strings(header)
        body
      })
  })

# Apply clean_strings to adjusted_headers_su to header row only
adjusted_headers_su <- adjusted_headers_su %>%
  map_if(is.data.frame, ~ {
    header <- names(.x)
    body <- .x
    names(body) <- clean_strings(header)
    body
  })
  

# Define a function to rename the columns of each tibble
rename_columns <- function(tibble) {
  tibble %>%
        rename_with(~ make_clean_names(str_remove(.x, paste0(q_pattern, "\\s"))))
    }

# Apply rename_columns function to each tibble in adjusted_headers
renamed_headers <- lapply(adjusted_headers, function(sublist) {
  lapply(sublist, rename_columns)
})

# Apply rename_columns function to each tibble in adjusted_headers_su
renamed_headers_su <- lapply(adjusted_headers_su, rename_columns)



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



### 4 - Check headers of substance use raw data have all been renamed, flag those which haven't ----

# Check all headers have been renamed
# Create an empty list to store the results
missing_headers_list_su <- list()

# Iterate through each list of tibbles in renamed_headers
for (i in seq_along(renamed_headers_su)) {
  # Get the name of the current list
  list_name_su <- names(renamed_headers_su)[i]
  
  # Create a list to store the missing headers for the current list
  list_missing_headers_su <- list()
  
  # Iterate through each tibble in the current list
  for (j in seq_along(renamed_headers_su[[i]])) {
    # Get the name of the current tibble
    tibble_name <- names(renamed_headers_su[[i]])[j]
    
    # Extract the current tibble from renamed_headers
    current_renamed_tibble <- renamed_headers_su[[i]][[j]]
    
    # Find the corresponding tibble in exp_headers by name
    current_exp_tibble <- exp_headers_su[[tibble_name]]
    
    # Get the values in the "h" column of the current exp_tibble
    exp_h_values <- current_exp_tibble$h
    
    # Find headers in current_renamed_tibble that are not in exp_h_values
    missing_headers_su <- setdiff(colnames(current_renamed_tibble), exp_h_values)
    
    # Store the missing headers for the current tibble
    list_missing_headers_su[[tibble_name]] <- missing_headers_su
  }
  
  # Store the missing headers for the current list
  missing_headers_list_su[[list_name_su]] <- list_missing_headers_su
}



### 5 - Rename column headers in adjusted_headers and adjusted_headers_su so they match the column "code" in exp_headers ----

# Function to rename tibble column headers
rename_headers <- function(tibble, exp_tibble) {
  recode_vec <- setNames(exp_tibble$h, exp_tibble$code)
  rename(tibble, any_of(recode_vec))
}

# Loop through renamed_headers and rename the columns using exp_headers
for (list_name in names(renamed_headers)) {
  for (tibble_name in names(renamed_headers[[list_name]])) {
    renamed_headers[[list_name]][[tibble_name]] <- rename_headers(
      renamed_headers[[list_name]][[tibble_name]], 
      exp_headers[[tibble_name]]
    )
  }
}

# Loop through renamed_headers_su and rename the columns using exp_headers_su
for (tibble_name in names(renamed_headers_su)) {
  renamed_headers_su[[tibble_name]] <- rename_headers(
    renamed_headers_su[[tibble_name]], 
    exp_headers_su
  )
}



### 6 - Check headers of renamed_headers have all been renamed, flag those which haven't ----

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
    
    # Get the values in the "code" column of the current exp_tibble
    exp_code_values <- current_exp_tibble$code
    
    # Find headers in current_renamed_tibble that are not in exp_code_values
    missing_headers <- setdiff(colnames(current_renamed_tibble), exp_code_values)
    
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



### 7 - Check headers of renamed_headers_su have all been renamed, flag those which haven't ----

# Check all headers have been renamed
# Create an empty list to store the results
missing_headers_list_su <- list()

# Iterate through each list of tibbles in renamed_headers_su
for (i in seq_along(renamed_headers_su)) {
  # Get the name of the current list
  list_name <- names(renamed_headers_su)[i]
  
  # Create a list to store the missing headers for the current list
  list_missing_headers <- list()
  
  # Iterate through each tibble in the current list
  for (j in seq_along(renamed_headers_su[[i]])) {
    # Get the name of the current tibble
    tibble_name <- names(renamed_headers_su[[i]])[j]
    
    # Extract the current tibble from renamed_headers_su
    current_renamed_tibble <- renamed_headers_su[[i]][[j]]
    
    # Find the corresponding tibble in exp_headers by name
    current_exp_tibble <- exp_headers[[tibble_name]]
    
    # Get the values in the "code" column of the current exp_tibble
    exp_code_values <- current_exp_tibble$code
    
    # Find headers in current_renamed_tibble that are not in exp_code_values
    missing_headers <- setdiff(colnames(current_renamed_tibble), exp_code_values)
    
    # Store the missing headers for the current tibble
    list_missing_headers[[tibble_name]] <- missing_headers
  }
  
  # Store the missing headers for the current list
  missing_headers_list_su[[list_name]] <- list_missing_headers
}


# Print only the tibbles with differing headers
for (tibble_name in names(list_missing_headers)) {
  missing_headers <- list_missing_headers[[tibble_name]]
  if (!identical(missing_headers, character(0))) {
    cat("Tibble:", tibble_name, "| Missing Headers:", toString(missing_headers), "\n")
  }
}



### 8 - Save renamed headers ----

# Save csv file to output folder
# Loop through each LA and list of stage tibbles
for (folder_name in all_las) {
  # Get the list of tibbles for the current LA
  tibble_list <- renamed_headers[[folder_name]]
  
  # Loop through each tibble in the list
  for (tibble_name in names(tibble_list)) {
    # Create the file name for the csv file
    file_name <- file.path(raw_data_folder, year, folder_name, "03_renamed_headers", 
                           paste0(folder_name, "_", tibble_name, ".csv"))
    
    # Get the tibble
    tibble_data <- tibble_list[[tibble_name]]
    
    # Save the tibble as an csv file
    write.csv(tibble_data, file_name, row.names = FALSE)
  }
}


# Save csv file to output folder
# Loop through each LA 
for (folder_name in names(renamed_headers_su)) {
  # Get the list of tibbles for the current LA
  tibble_name <- renamed_headers_su[[folder_name]]
  
    # Create the file name for the csv file
    file_name <- file.path(raw_data_folder, year, folder_name, "03_renamed_headers_substance_use", 
                           paste0(folder_name, "_S4_substance_use.csv"))
    
    # Save the tibble as an csv file
    write.csv(tibble_name, file_name, row.names = FALSE)
}

  
### END OF SCRIPT ###
