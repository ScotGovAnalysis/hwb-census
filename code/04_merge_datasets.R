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
source_data <- 
  map(all_las, function(las) {
    map(all_stages, function(stage) {
      read_raw_data(year, las, stage, "03_renamed_headers")
    })
  })


# Rename elements within source_data to respective local authority
names(source_data) <- c(all_las)


# Create function to rename each tibble within list of LA's to correct stage
rename_tibbles <- function(lst) {
  names(lst) <- all_stages
  return(lst)
}


# Rename elements within source_data to respective stage
source_data <- lapply(source_data, rename_tibbles)




### 2 - Insert new column at the beginning of each dataset with local authority name ----

# Add "hwb_la" column to the beginning of each tibble with the name of the outer list
modified_list <- source_data %>%
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
for (name in all_stages) {
  
  # Extract tibbles with the current name from modified_list
  tibbles_with_name <- lapply(modified_list, function(sub_list) sub_list[[name]])
  
  # Combine tibbles row-wise using bind_rows
  merged_tibble <- bind_rows(tibbles_with_name)
  
  # Add the merged tibble to merged_data
  merged_data[[name]] <- merged_tibble
}




### 4 - Save as excel file to  ----
write_xlsx(
  merged_data, 
  here("output", year, paste0(year, "_04_merged_data.xlsx"))
)


### END OF SCRIPT ###


