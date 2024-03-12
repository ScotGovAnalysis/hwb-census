#########################################################################
# Name of file - 04_merge_datasets.R
# Health and Wellbeing (HWB) Census 
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 4.2.2
#
# Description - Merge local authority datasets together for each stage
# and output as excel file

#########################################################################


### 0 - Setup ----

source(here::here("code", "00_setup.R"))
source(here::here("functions", "read_raw_data.R"))



### 1 - Read in local authority datasets ----

# Read in each dataframe from 03_renamed_headers for each LA and stage
# Store the data in a nested list structure
raw_data <- setNames(
  lapply(
    map(all_las, function(las) {
      map(all_stages, function(stage) {
        # Display a message indicating which file is currently being read
        cli_inform(sprintf("Reading data for local authority '%s' and stage '%s'", las, stage))
        
        # Read raw data for the current combination of local authority and stage
        read_raw_data(year, las, stage, "03_renamed_headers")
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
    read_raw_data(year, las, "S4", "03_renamed_headers_substance_use")
  }),
  # Set the names of tibble to the local authority names
  all_las
)


# Add each tibble from raw_data_su to each respectively named sublist of tibbles in raw_data, with the name "S4_SU"
# Loop through each tibble in raw_data_su
for (i in seq_along(raw_data_su)) {
  # Extract the ith tibble from raw_data_su
  tibble_to_add <- raw_data_su[[i]]
  
  # Add the tibble to the corresponding sublist in raw_data
  raw_data[[i]]$S4_SU <- tibble_to_add
}



### 2 - Insert new column at the beginning of each dataset with local authority name ----

# Add "hwb_la" column to the beginning of each tibble with the name of the outer list
modified_list <- raw_data %>%
  imap(function(sub_list, list_name) {
    sub_list %>%
      map(~ .x %>%
            mutate(hwb_la = list_name, .before = 1))
  })



### 3 - Merge datasets together by stage ----

# Function to convert all columns of a tibble to character
convert_to_character <- function(tibble) {
  tibble %>% 
    mutate(across(everything(), as.character))
}

# Apply the function to each tibble within each list in modified_list
modified_list <- modified_list %>%
  map(~ map(., convert_to_character))


# Initialize an empty list to store the merged tibbles
merged_data <- list()


# Iterate through the tibble names
for (name in survey_names) {
  
  # Extract tibbles with the current name from modified_list
  tibbles_with_name <- lapply(modified_list, function(sub_list) sub_list[[name]])
  
  # Combine tibbles row-wise using bind_rows
  merged_tibble <- bind_rows(tibbles_with_name)
  
  # Add the merged tibble to merged_data
  merged_data[[name]] <- merged_tibble
}



### 4 - Save as excel file to Merged folder ----

write_xlsx(
  merged_data,
  file.path(raw_data_folder, year, "Merged", paste0("04_merged_data.xlsx"))
)


### END OF SCRIPT ###
