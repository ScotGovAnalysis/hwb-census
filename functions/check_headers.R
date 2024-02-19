check_headers <- function(year, la, stage, subfolder, exp_headers, q_pattern) {
  
  cli_inform(c("i" = "Checking headers: {la}, {stage}"))
  
  # Check exp_headers in correct format
  if(!is.data.frame(exp_headers)) {
    cli_abort(c("x" = "`exp_headers` must be a data frame."))
  }
  if(!identical(names(exp_headers), c("h", "q", "n"))) {
    cli_abort(
      c("x" = "`exp_headers` must have three columns; 'h', 'q' and 'n'.")
    )
  }
  
  # Source required functions
  source(here("functions", "read_raw_data.R"))
  source(here("functions", "clean_strings.R"))
  source(here("functions", "merge_headers.R"))
  
  # Read in raw data to get headers and first row (for possible second header)
  raw <- read_raw_data(year, la, stage, subfolder, headers_only = TRUE)

  # Get number of header rows
  # If the first column answer is NA or the first header is ..1 or the first row contains a q_pattern
  # then there are likely to be two header rows
  n_headers <- if_else(is.na(raw[1, 1]) | names(raw)[1] == "...1" | grepl(q_pattern, raw[1, 1]), 2, 1)
  
  # Restructure data to get columns of headers
  raw_headers <- 
    
    # Restructure to long format and keep headers only
    pivot_longer(raw, cols = everything(), names_to = "h", values_to = "h2") |>
    select(all_of(1:n_headers)) |>
    
    # Recode auto-filled headers with NA
    mutate(across(everything(), ~ if_else(str_detect(., "^\\.{3}\\d+$"),
                                          NA_character_,
                                          .)))
  
  # Merge headers (if split over two rows)
  if(n_headers == 2) {
    raw_headers <- raw_headers |>
      mutate(h = merge_headers(h, h2)) |>
      select(-h2)
  }
  
  # Separate question number from header name
  raw_headers <- raw_headers |>
    mutate(
      q = str_extract(h, q_pattern),
      h = str_remove(h, paste0(q_pattern, "\\s"))
    ) |>
    # Flag duplicate names
    group_by(h) |>
    mutate(n = n()) |>
    ungroup()
  
  # Match headers from raw data to expected header data
  match <- 
    full_join(raw_headers, exp_headers, 
              by = "h", suffix = c("_raw", "_exp"),
              relationship = "many-to-many") |>
    
    # Flag issues
    mutate(
      issue = case_when(
        n_exp > 0 & is.na(n_raw) ~ "missing",
        n_raw > 0 & is.na(n_exp) ~ "extra",
        n_exp > n_raw ~ paste(n_exp - n_raw, "fewer than expected"),
        n_exp < n_raw ~ paste(n_raw - n_exp, "more than expected")
      )
    ) |>
    
    # Remove rows where no issue flagged
    filter(!is.na(issue)) |>
    
    # List question numbers where duplicate header names
    group_by(across(!matches("^q_(raw|exp)"))) |>
    summarise(across(matches("^q_(raw|exp)"),
                     \(x) toString(unique(x)) |> na_if("NA")),
              .groups = "drop") |>
    
    # Reorder columns and add identifiers
    select(issue, h, matches("^q_"), matches("^n_")) |>
    mutate(year = year,
           la = la,
           stage = toupper(stage),
           .before = everything())
  
  # Return data
  match
  
}
