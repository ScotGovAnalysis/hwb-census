
perform_analysis_local_authority_wemwbs <- function(hwb_analysis, variables) {
  
  # Filtering hwb_analysis_caring
  hwb_analysis_caring <- hwb_analysis %>%
    filter(pc_stage != "P5" & pc_stage != "P6")
  
  one_characteristics <- data.frame(
    dataset = c("hwb_analysis", "hwb_analysis", "hwb_analysis", "hwb_analysis", "hwb_analysis", "hwb_analysis_caring", "hwb_analysis"),
    var = c("pc_stage", "Gender", "SIMD2020v2_Quintile", "UrbRur6", "EthnicBackground", "care_for_someone", "long_term_condition")
  )
  
  # Analysis on one characteristic
  one_characteristic_list <- lapply(1:nrow(one_characteristics), function(j) {
    dataset <- one_characteristics$dataset[j]
    var <- one_characteristics$var[j]
    var_result <- lapply(1:nrow(variables), function(i) {
      analysis_wemwbs_one_characteristic(get(dataset), !!as.name(var))
    }) %>%
      bind_rows()
    
    as_tibble(var_result)
  })
  
  names(one_characteristic_list) <- one_characteristics$var
  
  # Organize results into combined_list
  combined_list <- list(stage = one_characteristic_list$pc_stage,
                        sex = one_characteristic_list$Gender,
                        simd = one_characteristic_list$SIMD2020v2_Quintile,
                        urbrur6 = one_characteristic_list$UrbRur6,
                        ethnic_group = one_characteristic_list$EthnicBackground,
                        care_for_someone = one_characteristic_list$care_for_someone,
                        long_term_condition = one_characteristic_list$long_term_condition)
  
  return(combined_list)
}