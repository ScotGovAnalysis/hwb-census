
# Create new folders in output folder

new_output_folders <- function(year, la, output_folder) {
  
  dir <- paste0(
    
    output_folder, "/",
    
    year, "/", la
  )
  
  if(!dir.exists(dir)) dir.create(dir)
  
  folders <- paste0(
    dir, "/", 
    c("Output", "Suppressed and formatted")
  )
  
  purrr::walk(
    folders,
    \(x) if(!dir.exists(x)) dir.create(x)
  )

}
