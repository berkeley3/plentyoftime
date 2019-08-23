my_glm_svy <- function(df=NULL, var, event, design = NULL, family = NULL, effect = NULL, 
                       firth = FALSE, weights){
  
  if (!firth){
    mod <- svyglm(as.formula(paste0(event ,' ~', var)), 
                  family = family,
                  design = design)
    resout <- display_svy(summary(mod), effect = effect, row_name = event, 
                          family = family, firth = firth)
  }else{
    mod <- logistf(as.formula(paste0(event ,' ~', var)), 
                   data = df,
                   weights = weights)
    resout <- display_svy(mod, effect = effect, row_name = event, 
                          family = family, firth = firth)
  }
  
  
  resout
  
}
