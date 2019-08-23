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


my_lrm <- function(df, var, event, confounders=NULL, show_diff = FALSE, 
                   show_confounders = FALSE, firth = FALSE, effect = 'OR'){
  options(datadist='dd')
  options(warn=-1)
  
  df2 <- df %>%dplyr::select(var, event, confounders)
  
  
  if(!firth){
    
   dd <<- rms::datadist(df2)
    
  if(is.null(confounders)){
    mod <- rms::lrm(as.formula(paste0(event ,' ~', var)), 
               data = df2, x = T, y = T)}else{
                 mod <- rms::lrm(as.formula(paste0(event,' ~', var, '+', paste0(confounders,collapse = '+'))), 
                            data = df2, x = T, y = T)             
               }
  
  if(show_confounders) {plentyoftime::display(summary(mod), diff= show_diff, effect = effect)}else{
    resout <- plentyoftime::display(summary(mod), diff= show_diff, effect = effect)
    idx <- grep(var, rownames(resout))
    resout <- resout[idx,]
    resout
  }
  }else{
    
    
    df2 = na.omit(df2)
    if(is.null(confounders)){
      mod <- logistf(as.formula(paste0(event ,' ~', var)),
                    data = df2)}else{
                      mod <- coxphf(as.formula(paste0(event,' ~', var, '+', paste0(confounders,collapse = '+'))),
                                    data = df2)
                    }
    
    if(show_confounders) {display(mod, show_diff= show_diff, effect = effect, firth = T)}else{
      resout <- display(mod, show_diff= show_diff, effect = effect, firth = T)
      idx <- grep(var, rownames(resout))
      resout <- resout[idx,]
      resout
    }
    
    
  }
  
}