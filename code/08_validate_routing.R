#########################################################################
# Name of file - 08_validate_routing.R
# Health and Wellbeing (HWB) Census 
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 4.2.2
#
# Description - Some local authorities changed the routing (skip logic) in SmartSurvey. 
# This code compares each survey against the respective metadata template for routing.
# Questions which were asked of students which shouldn't have been will instead
# get replaced with NA.
#########################################################################


### 0 - Setup ----

source(here::here("code", "00_setup.R"))
source(here::here("functions", "read_raw_data.R"))



### 1 - Read in routing metadata ----

# Read in expected routing metadata for each survey and store as a list of data frames
routing_metadata <- set_names(
  map(
    survey_names, 
    ~ read_xlsx(here(
      "metadata",
      year,
      "hwb_metadata_routing.xlsx"), sheet = .x)
  ),
  survey_names
)



### 2 - Read in raw data ----

# Read in all sheets into a list of tibbles with names 
raw_data <- 
  map(
    survey_names,
    ~ read_xlsx(file.path(raw_data_folder, year, "Merged", "05_validated_rows.xlsx"), sheet = .x)
  )



### 3 - Replace routing in raw_data with routing in routing_metadata ----

# Iterate through each survey defined in survey_names
for (survey in survey_names) {
  # Iterate through each row within the current survey's routing metadata
  for (row in 1:length(routing_metadata[[survey]]$question)) {
    # Extract the R expression as a string
    expr_string <- routing_metadata[[survey]]$routing[row]
    col_name <- as.name(routing_metadata[[survey]]$question[row])
    
    # Modify the raw_data within the current survey
    raw_data[[survey]] <- raw_data[[survey]] %>%
      # Evaluate the R expression and return a logical vector
      # If the expression is TRUE, set the column value to NA
      # Otherwise, keep the original column value
      mutate(!!col_name := ifelse(eval(parse(text = expr_string)), NA, !!col_name))
  }
}



### 4 - Save as excel file to Merged folder ----

# Save excel file to output folder
write_xlsx(
  raw_data,
  file.path(raw_data_folder, year, "Merged", paste0("06_validated_routing.xlsx"))
)


### END OF SCRIPT ###
