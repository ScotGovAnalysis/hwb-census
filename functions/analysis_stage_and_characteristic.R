analysis_stage_and_characteristic <- function(var, que, que_quo, cat) {
  a <- left_join(
    rbind(
      (
        hwb_analysis %>% 
          select(pc_stage, {{var}}, {{que}}) %>% 
          group_by(pc_stage, {{var}}) %>%  
          count({{que}}) %>%  
          pivot_wider(names_from = c(pc_stage, {{var}}), values_from = n) %>% 
          filter({{que}} != "NA" & {{que}} != "Data not collected" & {{que}} != "Not applicable" & {{que}} != "Question not asked of stage") %>% 
          adorn_percentages("col") %>% 
          adorn_pct_formatting(digits = 1, rounding = "half to even", affix_sign = FALSE)
      ),
      (
        hwb_analysis %>% 
          select(pc_stage, {{var}}, {{que}}) %>% 
          group_by(pc_stage, {{var}}) %>% 
          count({{que}}) %>%  
          pivot_wider(names_from = c(pc_stage, {{var}}), values_from = n) %>% 
          filter({{que}} != "NA" & {{que}} != "Data not collected" & {{que}} != "Not applicable" & {{que}} != "Question not asked of stage") %>% 
          adorn_totals() %>% 
          slice_tail()
      )
    ),
    rbind(
      (
        hwb_analysis %>% 
          select({{var}}, {{que}}) %>%
          group_by({{var}}) %>% 
          count({{que}}) %>% 
          pivot_wider(names_from = c({{var}}), values_from = n) %>% 
          filter({{que}} != "NA" & {{que}} != "Data not collected" & {{que}} != "Not applicable" & {{que}} != "Question not asked of stage") %>% 
          adorn_percentages("col") %>% 
          adorn_pct_formatting(digits = 1, rounding = "half to even", affix_sign = FALSE)
      ),
      (
        hwb_analysis %>% 
          select({{var}}, {{que}}) %>%
          group_by({{var}}) %>% 
          count({{que}}) %>%
          pivot_wider(names_from = c({{var}}), values_from = n) %>%
          filter({{que}} != "NA" & {{que}} != "Data not collected" & {{que}} != "Not applicable" & {{que}} != "Question not asked of stage") %>%
          adorn_totals() %>% 
          slice_tail()
      )
    ),
    by = que_quo
  ) 
  
  b <- rbind(
    (
      hwb_analysis %>% 
        filter({{que}} != "NA" & {{que}} != "Data not collected" & {{que}} != "Not applicable" & {{que}} != "Question not asked of stage") %>%
        count({{que}}) %>%
        adorn_percentages("col") %>% 
        adorn_pct_formatting(digits = 1, rounding = "half to even", affix_sign = FALSE)
    ),
    (
      hwb_analysis %>% 
        filter({{que}} != "NA" & {{que}} != "Data not collected" & {{que}} != "Not applicable" & {{que}} != "Question not asked of stage") %>%
        count({{que}}) %>%
        adorn_totals() %>% 
        slice_tail())
  ) %>%
    rename(`Total %` = n)
  
  c <- bind_cols(a, select(b, -intersect(names(a), names(b)))) %>% 
    mutate("Survey Question" = que_quo) %>% 
    rename("Response" = que_quo) %>% 
    arrange(match(Response, cat))
  
  # Move "Survey Question" to the first column position
  d <- c[, c("Survey Question", setdiff(names(c), "Survey Question"))]
  
  return(d)
}
