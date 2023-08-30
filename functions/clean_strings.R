clean_strings <- function(x) {
  
  if(!inherits(x, "character")) {
    stop("x must be a character")
  }
  
  # 'Other format' unicode characters
  str_remove_all(x, "\\p{Cf}+") |>
    
    # Return characters
    str_remove_all("\\r") |>
    str_remove_all("\\n") |>
    
    # Remove characters that aren't a word, space, . or digit
    str_remove_all("[^\\w\\s\\.\\d]")
  
}
