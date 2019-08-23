my_glm <- function(df=NULL, var, event, family = NULL, effect = NULL, 
                   firth = FALSE){
  
  if (!firth){
    mod <- glm(as.formula(paste0(event ,' ~', var)), 
               family = family,
               data = df)
    resout <- display_glm(summary(mod), effect = effect, row_name = event, 
                          family = family, firth = firth)
  }else{
    mod <- logistf(as.formula(paste0(event ,' ~', var)), 
                   data = df)
    resout <- display_glm(mod, effect = effect, row_name = event, 
                          family = family, firth = firth)
  }
  
  
  resout
  
}
