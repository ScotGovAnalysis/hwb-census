analysis_stage_and_characteristic <- function(dataset, var, que, que_quo, cat_order) {
  cat_vector <- get(cat_order)
  
  a <- left_join(
    rbind(
      (
        dataset %>% 
          select(pc_stage, {{var}}, {{que}}) %>% 
          group_by(pc_stage, {{var}}) %>%  
          count({{que}}) %>%  
          pivot_wider(names_from = c(pc_stage, {{var}}), values_from = n) %>% 
          filter(!is.na({{que}}) & {{que}} != "Data not collected" & {{que}} != "Not applicable" & {{que}} != "Question not asked of stage") %>% 
          adorn_percentages("col") %>% 
          adorn_pct_formatting(digits = 1, rounding = "half to even", affix_sign = FALSE)
      ),
      (
        dataset %>% 
          select(pc_stage, {{var}}, {{que}}) %>% 
          group_by(pc_stage, {{var}}) %>% 
          count({{que}}) %>%  
          pivot_wider(names_from = c(pc_stage, {{var}}), values_from = n) %>% 
          filter(!is.na({{que}}) & {{que}} != "Data not collected" & {{que}} != "Not applicable" & {{que}} != "Question not asked of stage") %>% 
          adorn_totals() %>% 
          slice_tail()
      )
    ),
    rbind(
      (
        dataset %>% 
          select({{var}}, {{que}}) %>%
          group_by({{var}}) %>% 
          count({{que}}) %>% 
          pivot_wider(names_from = c({{var}}), values_from = n) %>% 
          filter(!is.na({{que}}) & {{que}} != "Data not collected" & {{que}} != "Not applicable" & {{que}} != "Question not asked of stage") %>% 
          adorn_percentages("col") %>% 
          adorn_pct_formatting(digits = 1, rounding = "half to even", affix_sign = FALSE)
      ),
      (
        dataset %>% 
          select({{var}}, {{que}}) %>%
          group_by({{var}}) %>% 
          count({{que}}) %>%
          pivot_wider(names_from = c({{var}}), values_from = n) %>%
          filter(!is.na({{que}}) & {{que}} != "Data not collected" & {{que}} != "Not applicable" & {{que}} != "Question not asked of stage") %>%
          adorn_totals() %>% 
          slice_tail()
      )
    ),
    by = que_quo
  ) 
  
  b <- rbind(
    (
      dataset %>% 
        filter(!is.na({{que}}) & {{que}} != "Data not collected" & {{que}} != "Not applicable" & {{que}} != "Question not asked of stage") %>%
        count({{que}}) %>%
        adorn_percentages("col") %>% 
        adorn_pct_formatting(digits = 1, rounding = "half to even", affix_sign = FALSE)
    ),
    (
      dataset %>% 
        filter(!is.na({{que}}) & {{que}} != "Data not collected" & {{que}} != "Not applicable" & {{que}} != "Question not asked of stage") %>%
        count({{que}}) %>%
        adorn_totals() %>% 
        slice_tail())
  ) %>%
    rename(`Total %` = n)
  
  c <- bind_cols(a, select(b, -intersect(names(a), names(b)))) %>% 
    mutate("Survey question" = que_quo) %>% 
    rename("Response" = que_quo) %>% 
    arrange(match(Response, cat_vector))
  
  # Move "Survey question" to the first column position
  d <- c[, c("Survey question", setdiff(names(c), "Survey question"))]
  
  return(d)
}
