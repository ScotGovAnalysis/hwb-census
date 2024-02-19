#########################################################################
# Name of file - 08_validate_routing.R
# Health and Wellbeing (HWB) Census 
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 4.2.2
#
# Description - Some local authorities changed the routing (skip logic) in SmartSurvey. 
# This code compares each stage against the respective metadata template for routing 
# Questions which were asked of students which shouldn't have been will instead
# get replaced with NA
#########################################################################


### 0 - Setup ----

source(here::here("code", "00_setup.R"))
source(here::here("functions", "read_raw_data.R"))



### 1 - Read in routing metadata ----

# For HWB data
routing_metadata <- set_names(
  map(
    all_stages, 
    ~ read_xlsx(here(
      "metadata",
      year,
      "hwb_metadata_routing.xlsx"), sheet = .x)
  ),
  all_stages
)

# For substance use
routing_metadata_su <- read_xlsx(here(
  "metadata",
  year,
  "hwb_metadata_routing_substance_use.xlsx"))




### 2 - Read in raw data ----

# Read in all sheets into a list of tibbles for HWB data
raw_data <- set_names(
  map(
    all_stages,
    ~ read_xlsx(file.path(raw_data_folder, year, "Merged", "05_validated_rows.xlsx"), sheet = .x)
  ),
  all_stages
)

# Read in substance use data as a dataframe
raw_data_su <- read_xlsx(file.path(raw_data_folder, year, "Merged", "05_validated_rows_substance_use.xlsx"))



### 3 - Replace routing in raw_data with routing in routing_metadata ----

# For HWB data
# Iterate through each stage defined in all_stages
for (stage in all_stages) {
  # Iterate through each row within the current stage's routing metadata
  for (row in 1:length(routing_metadata[[stage]]$question)) {
    # Extract the R expression as a string
    expr_string <- routing_metadata[[stage]]$routing[row]
    col_name <- as.name(routing_metadata[[stage]]$question[row])
    
    # Modify the raw_data within the current stage
    raw_data[[stage]] <- raw_data[[stage]] %>%
      # Evaluate the R expression and return a logical vector
      # If the expression is TRUE, set the column value to NA
      # Otherwise, keep the original column value
      mutate(!!col_name := ifelse(eval(parse(text = expr_string)), NA, !!col_name))
  }
}


# For substance use data
# Iterate through each row within the routing metadata
for (row in 1:length(routing_metadata_su$question)) {
  # Extract the R expression as a string
  expr_string <- routing_metadata_su$routing[row]
  col_name <- as.name(routing_metadata_su$question[row])
  
  # Modify raw_data_su
  raw_data_su <- raw_data_su %>%
    # Evaluate the R expression and return a logical vector
    # If the expression is TRUE, set the column value to NA
    # Otherwise, keep the original column value
    mutate(!!col_name := ifelse(eval(parse(text = expr_string)), NA, !!col_name))
}


  
### 4 - Save as excel file to Merged folder ----

# For HWB data
write_xlsx(
  raw_data,
  file.path(raw_data_folder, year, "Merged", paste0("06_validated_routing.xlsx"))
)

# For substance use data
write_xlsx(
  raw_data_su,
  file.path(raw_data_folder, year, "Merged", paste0("06_validated_routing_substance_use.xlsx"))
)


### END OF SCRIPT ###


