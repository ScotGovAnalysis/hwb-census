
# Create new folders in raw_data directory

new_folders <- function(year, la, raw_data_folder) {
  
  dir <- paste0(
    raw_data_folder,
    year, "/", la
  )
  
  if(!dir.exists(dir)) dir.create(dir)
  
  folders <- paste0(
    dir, "/", 
    c("Archive", "Substance Use", "01_submitted_data", "02_validated_headers", "03_renamed_headers")
  )
  
  purrr::walk(
    folders,
    \(x) if(!dir.exists(x)) dir.create(x)
  )
  
}
