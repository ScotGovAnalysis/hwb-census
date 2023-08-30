merge_headers <- function(h1, h2) {
  
  if(length(h1) != length(h2)) {
    stop("h1 and h2 must be the same length.")
  }  
  
  # Define sub-question pattern; e.g. Q1.1. (not Q1.)
  q_sub  <- "^Q\\d{1,2}\\.(\\d{1,2}\\.)\\s"
  
  h <- 
    bind_cols(h1 = h1, h2 = h2) |>
    mutate(merged = case_when(
      str_detect(h1, q_sub) | is.na(h2) ~ h1,
      str_detect(h2, q_sub) | is.na(h1) ~ h2
    ))
  
  h$merged
  
}
