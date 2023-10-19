
# Create new folders in raw_data directory for merged datasets

new_folders_merged <- function(year, raw_data_folder) {
  
  dir_merged <- paste0(raw_data_folder, year, "/Merged")
  
  if(!dir.exists(dir_merged)) dir.create(dir_merged)
  
}