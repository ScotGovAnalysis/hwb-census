
# Create new folders in raw_data directory

new_folders <- function(year, la, raw_data_folder) {
  
  dir <- paste0(

    raw_data_folder, "/",

    year, "/", la
  )
  
  if(!dir.exists(dir)) dir.create(dir)
  
  folders <- paste0(
    dir, "/", 
    c("Archive", "01_submitted_data", "01_submitted_data_substance_use", "02_validated_headers", "02_validated_headers_substance_use", 
      "03_renamed_headers", "03_renamed_headers_substance_use")
  )
  
  purrr::walk(
    folders,
    \(x) if(!dir.exists(x)) dir.create(x)
  )
  
}