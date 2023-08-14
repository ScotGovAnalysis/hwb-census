
# Create new folders in raw_data directory

new_folders <- function(year, la) {
  
  dir <- paste0(
    "//s0196a/ADM-Education-NIF Analysis/Health and Wellbeing Survey",
    "/R/RAP Project/raw_data/", 
    year, "/", la
  )
  
  archive <- paste0(dir, "/Archive")
  sub_use <- paste0(dir, "/Substance Use")
  
  if(!dir.exists(dir))     dir.create(dir)
  if(!dir.exists(archive)) dir.create(archive)
  if(!dir.exists(sub_use)) dir.create(sub_use)
  
}
