#' Generate dataframe from a summary logistf model or a summary rms::lrm/rms::cph model to be passed to kable
#'
#' @param summry.model A summary of a logistif model or a rms::lrm/rms::cph model
#' @param digits The number of digits to be returned. Default is 3
#' @param show_diff Should the diff column (inter-quartile-range odds/hazard ratio) be shown for rms::lrm/rms::cph summary models. Default is FALSE
#' @param effect Character indicating which kind of effect (OR or HR). Default is NULL, indicating a column named Effect
#' @param firth Logical value indicating whether the summary is applied to logistf model (firth = T) or a lrm/cph model (firth = F). Default is F
#'
#' @return A dataframe to pass to kable
#'
#' @examples
#'
#' @export
#'


display2 <- function(
  summary.model, digits = 3, show_diff = FALSE, effect=NULL, firth = FALSE
){
  if(!firth){
    n <- nrow(summary.model)
    idx <- seq(1, n, by = 2)
    m <- matrix(summary.model, nrow = n)
    pval <- 2*pnorm(abs(m[idx, 4]/m[idx, 5]), lower.tail = F)
    results <- round(matrix(m[idx+1, c(1:4,6:7)], nrow = n/2),digits)
    rownames(results) <- rownames(summary.model)[idx]
    colnames(results) <- c('Low', 'High', 'Diff', 'Effect', 'Lower 0.95', 'Upper 0.95')
    results <- data.frame(results, p = pval)
    results <- results[,3:ncol(results)]
    results$p <- ifelse(results$p < 0.001, '< 0.001', round(results$p,digits))
    if (!is.null(effect)) names(results)[grep('Effect', names(results))]= effect
    if(!show_diff)
      results <- results[,-1]
  }else{

    if(!show_diff){

      results = data.frame(Effect = round(as.numeric(exp(summary.model$coefficients)),digits),
                           Lower.0.95= round((summary.model$ci.lower), digits),
                           Upper.0.95 = round((summary.model$ci.upper), digits),
                           p = round(summary.model$prob,digits))
      results$p <- ifelse(results$p < 0.001, '< 0.001', results$p)

    }else{
      results = data.frame(Diff = NA,
                           Effect = round(as.numeric(exp(summary.model$coefficients)),digits),
                           Lower.0.95= round((summary.model$ci.lower), digits),
                           Upper.0.95 = round((summary.model$ci.upper), digits),
                           p = round(summary.model$prob,digits))
      results$p <- ifelse(results$p < 0.001, '< 0.001', results$p)

    }


    if (!is.null(effect)) names(results)[grep('Effect', names(results))]= effect
  }
  results
}




