#########################################################################
# Name of file - 01_validate_headers.R
# Health and Wellbeing (HWB) Census 
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 4.2.2
#
# Description - Validates headers of sumbitted data files and creates an
# excel file summarising issues by Local Authority and Stage
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
        # Remove la_code header (this is not included in raw data submissions)
        filter(code != "la_code") |>
        # Keep raw header columns only
        select(question_header1, question_header2) |>
        # Clean strings
        mutate(across(everything(), clean_strings)) |>
        # Use merged headers only
        select(h = question_header2) |>
        # Remove duplicates (remove this?)
        distinct() |>
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


### 2 - Check headers of raw data ----

# Run check_headers() for each LA and Stage
header_issues <- 
  pmap_dfr(
    expand_grid(all_las, all_stages),
    ~ check_headers(year, .x, .y, exp_headers[[.y]], q_pattern)
  )

# Summary of number of issues by LA and Stage
summary <- 
  header_issues |>
  count(la, stage) |>
  pivot_wider(names_from = stage, values_from = n, values_fill = 0) |>
  select(la, all_of(all_stages)) |>
  adorn_totals()


### 3 - Save header validation summary

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

# Save excel file to output folder
write_xlsx(
  header_issues_list, 
  here("output", year, paste0(year, "_header-validation.xlsx"))
)


### END OF SCRIPT ###