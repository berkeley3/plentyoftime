#' Run lpgistic regression adjusted model (rms::lrm function) and return a dataframe of the summary model
#' 
#' @param var Character name of the variable of intererest
#' @param event Character name of the response variable
#' @param confounders Character vector of variable confounders. Default is NULL, i.e. a univariate logistic model is run
#' @param show_diff Logical. Should the diff column (inter-quartile-range odds/hazard ratio) be shown for rms::lrm summary models. Default is FALSE
#' @param show_confounders Logical. Should estimate of confounding variables be shown. Default is FALSE, i.e. only var's OS is reported
#' @param effect  Character indicating the name of the column with the estimated effectR. Default is OR. If NULL Effect is reporeted
#' 
#' @return A dataframe to be passed to kable
#' 
#' @examples 
#' 
#' @export
#' 

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
