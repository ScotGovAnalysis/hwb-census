#########################################################################
# Name of file - 01_validate_headers.R
# Health and Wellbeing (HWB) Census 
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 4.2.2
#
# Description - Validates headers of submitted data files and creates an
# excel file summarising issues by local authority and stage
#########################################################################


### 0 - Setup ----

source(here::here("code", "00_setup.R"))



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
exp_headers <- exp_headers |>
  map(\(x)   
      x |>
        # Recode character NAs as system NAs
        mutate(across(where(is.character), ~ na_if(., "NA"))) |>
        # Remove headers added by SG (not included in raw data submissions)
        filter(!code %in% c("la_code", "seed_code")) |>
        # Keep raw header columns only
        select(question_header1, question_header2) |>
        # Clean strings
        mutate(across(everything(), clean_strings)) |>
        # Use merged headers only
        select(h = question_header2) |>
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

# Read in expected headers for substance use
exp_headers_su <- 
  read_xlsx(here(
    "metadata", 
    year,
    paste0("hwb_metadata_s4_su_column_names.xlsx")
  ))

# Clean and restructure expected headers for substance use
exp_headers_su <- exp_headers_su |>
  # Recode character NAs as system NAs
  mutate(across(where(is.character), ~ na_if(., "NA"))) |>
  # Remove headers added by SG (not included in raw data submissions)
  filter(!code %in% c("la_code", "seed_code")) |>
  # Keep raw header columns only
  select(question_header1, question_header2) |>
  # Clean strings
  mutate(across(everything(), clean_strings)) |>
  # Use merged headers only
  select(h = question_header2) |>
  # Extract question number
  mutate(
    q  = str_extract(h, q_pattern),
    h = str_remove(h, paste0(q_pattern, "\\s"))
  ) |>
  # Add count of duplicate headers
  group_by(h) |>
  mutate(n = n()) |>
  ungroup()



### 2 - Check headers of raw data ----

# Run check_headers() for each LA and stage
header_issues <- 
  pmap_dfr(
    expand_grid(all_las, all_stages),
    ~ check_headers(year, .x, .y, "01_submitted_data", exp_headers[[.y]], q_pattern)
  )

# Run check_headers() for each LA for S4 substance use
header_issues_su <- pmap_dfr(
  expand_grid(all_las),
  ~ check_headers(year, .x, "S4", "01_submitted_data_substance_use", exp_headers_su, q_pattern)
)
  

# Summary of number of issues by LA and stage
summary <- 
  header_issues |>
  count(la, stage) |>
  pivot_wider(names_from = stage, values_from = n, values_fill = 0) |>
  select(la, all_of(all_stages)) |>
  adorn_totals()

# Summary of number of issues by LA for substance use
summary_su <- 
  header_issues_su |>
  count(la, stage) |>
  pivot_wider(names_from = stage, values_from = n, values_fill = 0) |>
  select(la, "S4") |>
  adorn_totals()



### 3 - Save header validation summary ----

# Key for what each column in summary data means
key <- tibble(col = names(header_issues)) |>
  mutate(meaning = case_when(
    col == "year" ~ "Year",
    col == "la" ~ "Local Authority",
    col == "stage" ~ "Stage of school",
    col == "issue" ~ "Type of issue; e.g. extra column, missing column",
    col == "h" ~ "Header",
    col == "q_raw" ~ "Question number from raw data",
    col == "q_exp" ~ "Expected question number",
    col == "n_raw" ~ "Number of headers in raw data",
    col == "n_exp" ~ "Number of headers in expected data"
  ))

# Structure output as list - each element will be it's own sheet in excel file
header_issues_list <- c(
  list(Key = key, Summary = summary),
  split(header_issues, header_issues$la)
)

# Structure output as list - each LA will be it's own sheet in excel file for substance use
header_issues_list_su <- c(
  list(Key = key, Summary = summary_su),
  split(header_issues_su, header_issues_su$la)
)

# Save excel file to output folder
write_xlsx(
  header_issues_list, 
  here("output", year, paste0(year, "_header-validation.xlsx"))
)

# Save excel file to output folder for substance use
write_xlsx(
  header_issues_list_su, 
  here("output", year, paste0(year, "_header-validation_substance_use.xlsx"))
)


### END OF SCRIPT ###
