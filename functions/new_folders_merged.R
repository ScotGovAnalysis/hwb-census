
# Create new folders in raw_data directory for merged datasets

new_folders_merged <- function(year) {
  
  dir_merged <- paste0(
    "//s0196a/ADM-Education-NIF Analysis/Health and Wellbeing Survey",
    "/R/RAP Project/raw_data/", 
    year, "/Merged"
  )
  
  if(!dir.exists(dir_merged)) dir.create(dir_merged)
  
  folders_merged <- paste0(
    dir_merged, "/", 
    c("04_merged_data")
  )
  
  purrr::walk(
    folders_merged,
    \(x) if(!dir.exists(x)) dir.create(x)
  )
  
}