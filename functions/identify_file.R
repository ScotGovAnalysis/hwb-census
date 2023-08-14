identify_file <- function(year, la, stage) {
  
  # Define what stages are valid
  exp_stages <- c(paste0("P", 5:7), paste0("S", 1:6))
  
  # Return an error if invalid stage supplied
  if(!toupper(stage) %in% exp_stages) {
    stop("Invalid stage value supplied.\n", 
         "Accepted values: ", 
         paste(exp_stages, collapse = ", "), ".")
  }
  
  # Reformat stage for string matching
  stage_split <- stringr::str_split_1(stage, stringr::boundary("character"))
  
  stage_regex <- paste0(
    "[", toupper(stage_split[1]), tolower(stage_split[1]), "]", stage_split[2]
  )
  
  # Construct file path of folder to search for file
  dir <- paste0(
    "//s0196a/ADM-Education-NIF Analysis/Health and Wellbeing Survey",
    "/R/RAP Project/raw_data/", 
    year, "/", la
  )
  
  dir_short <- paste("raw_data", year, la, sep = "/")
  
  # Return error if folder doesn't exist
  if(!file.exists(dir)) {
    stop("Directory doesn't exist for supplied year and la.\n",
         "Check existing directories in raw_data/")
  }
  
  # List files in folder with stage match
  files <- 
    list.files(dir, pattern = stage_regex, full.names = TRUE) |>
    # Remove any non- csv or xlsx files
    stringr::str_subset("\\.(csv|xlsx)$")
  
  # Return error if no files found
  if(length(files) == 0) {
    stop("No csv or xlsx files found for ", stage, " stage in ", dir_short, ".")
  }
  
  # Return error if more than one file found
  if(length(files) > 1) {
    stop("More than one file found for ", stage, " stage in ", dir_short, ".\n",
         rlang::format_error_bullets(basename(files)))
  }
  
  # Return files
  files
  
}