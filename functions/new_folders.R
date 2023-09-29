
# Create new folders in raw_data directory

new_folders <- function(year, la) {
  
  dir <- paste0(
    "//s0196a/ADM-Education-NIF Analysis/Health and Wellbeing Survey",
    "/R/RAP Project/raw_data/", 
    year, "/", la
  )
  
  if(!dir.exists(dir)) dir.create(dir)
  
  folders <- paste0(
    dir, "/", 
    c("Archive", "Substance Use", "01_submitted_data", "02_validated_headers", "03_renamed_headers", "04_validated_rows")
  )
  
  purrr::walk(
    folders,
    \(x) if(!dir.exists(x)) dir.create(x)
  )
  
}
