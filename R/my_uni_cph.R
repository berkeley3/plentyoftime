#' Generate a dataframe from a summary of rms:::cph univariate model to be passed to kable for creating
#' a table of results of a univariate analysis
#'
#' @param df A data.frame
#' @param var character name of the covariate $X$ of interest in the univariate analysis
#' @param time_to_event character name of the time to event variable
#' @param event character name of the binary event variable
#' @param confounders character vector of names of covariates for which adjusting the analysis
#' @param show_diff Should the diff column (inter-quartile-range odds/hazard ratio) be shown for rms::lrm/rms::cph summary models. Default is FALSE
#' @param show.confounders logical. If TRUE the HRs of confounders are reported
#' @param effect Character indicating which kind of effect (OR or HR). Default is NULL, indicating a column named Effect
#' @param firth Logical value indicating whether the summary is applied to logistf model (firth = T) or a lrm/cph model (firth = F). Default is F
#'
#' @return A dataframe to be passed to kable
#'
#' @examples
#'
#' @export
#'






my_uni_cph <- function(df, var, time_to_event, event, confounders=NULL, show_diff = FALSE,
                       show.confounders = FALSE, effect = NULL, firth = FALSE){
  options(datadist='dd')
  options(warn=-1)

  if(is.null(confounders)){
    df2 <- df %>%dplyr::select(var, time_to_event, event)}else{
      df2 <- df %>% dplyr::select(var, time_to_event, event, confounders)
    }

  if(!firth){
    dd <<- datadist(df2)

    if(is.null(confounders)){
      mod <- cph(as.formula(paste0('Surv(', time_to_event,',', event,') ~', var)),
                 data = df2, x = T, y = T)}else{
                   mod <- cph(as.formula(paste0('Surv(', time_to_event,',', event,') ~', var, '+', paste0(confounders,collapse = '+'))),
                              data = df2, x = T, y = T)
                 }

    if(show.confounders) {display(summary(mod), show_diff= show_diff, effect = effect)}else{
      resout <- display(summary(mod), show_diff= show_diff, effect = effect)
      idx <- grep(var, rownames(resout))
      resout <- resout[idx,]
      resout
    }
  }else{
    df2 = na.omit(df2)
    if(is.null(confounders)){
      mod <- coxphf(as.formula(paste0('Surv(', time_to_event,',', event,') ~', var)),
                    data = df2)}else{
                      mod <- coxphf(as.formula(paste0('Surv(', time_to_event,',', event,') ~', var, '+', paste0(confounders,collapse = '+'))),
                                    data = df2)
                    }

    if(show.confounders) {display(mod, show_diff= show_diff, effect = effect, firth = T)}else{
      resout <- display(mod, show_diff= show_diff, effect = effect, firth = T)
      idx <- grep(var, rownames(resout))
      resout <- resout[idx,]
      resout
    }



  }



}

