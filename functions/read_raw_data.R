read_raw_data <- function(year, la, stage, subfolder, headers_only = FALSE) {
  
  source(here("functions", "clean_strings.R"))
  source(here("functions", "identify_file.R"))
  
  file <- identify_file(year, la, stage, subfolder)
  
  # Get file extension
  type <- tools::file_ext(file)
  
  # Guess encoding of file (some files do not use UTF-8 encoding)
  if(type == "csv") {
    enc <- 
      readr::guess_encoding(file) |>
      dplyr::slice_max(confidence, n = 1) |>
      pull(encoding)
  }
  
  # Define read data arguments
  args <- list(file)
  
  if(type == "csv") {
    args <- c(args, list(
      locale = locale(encoding = enc),
      show_col_types = FALSE
    ))
  }
  
  if(headers_only) {
    args <- c(args, list(
      n_max = 1,
      col_types = if(type == "csv") cols(.default = "c") else "text"
    ))
  }
    
  # Read in data
  dat <- 
    suppressMessages(do.call(
      # Use different read function depending on file type
      ifelse(type == "csv", readr::read_csv, readxl::read_xlsx),
      args
    )) |>
    # Remove empty rows
    janitor::remove_empty("rows")
  
  # Clean strings
  if(headers_only)
    dat <- dplyr::mutate(
      dat, 
      dplyr::across(dplyr::everything(), 
                    clean_strings)
    )
    
  names(dat) <- clean_strings(names(dat))
  
  # Return data
  dat

}
