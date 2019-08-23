display_glm <- function( summary.model, digits = 3, effect=NULL, family = NULL,
                         firth = FALSE, model=NULL, row_name = NULL){
  
  if(!firth){
    if(family!='gaussian'){
      
      coeff <- summary.model$coefficients[-1,1]
      se <- summary.model$coefficients[-1,2]
      pval <- se <- summary.model$coefficients[-1,4]
      
      results = data.frame(Effect = round(exp(coeff),digits), 
                           Lower.0.95= round(exp(coeff - 1.96*se), digits),
                           Upper.0.95 = round(exp(coeff + 1.96*se), digits), 
                           p = round(pval,digits))
      results$p <- ifelse(results$p < 0.001, '< 0.001', results$p)
      
      #   if (!is.null(effect)) names(results)[grep('Effect', names(results))]= effect
      
    }else{
      coeff <- summary.model$coefficients[-1,1]
      se <- summary.model$coefficients[-1,2]
      pval <- summary.model$coefficients[-1,4]
      
      results = data.frame(Effect = round(coeff,digits), 
                           Lower.0.95= round(coeff - 1.96*se, digits),
                           Upper.0.95 = round(coeff + 1.96*se, digits), 
                           p = round(pval,digits))
      results$p <- ifelse(results$p < 0.001, '< 0.001', results$p)
      
    }}else{
      
      results = data.frame(Effect = round(as.numeric(exp(summary.model$coefficients[-1])),digits), 
                           Lower.0.95= round(exp(summary.model$ci.lower[-1]), digits),
                           Upper.0.95 = round(exp(summary.model$ci.upper[-1]), digits), 
                           p = round(summary.model$prob[-1],digits))
      results$p <- ifelse(results$p < 0.001, '< 0.001', results$p)
      
    }
  
  
  
  if (!is.null(effect)) names(results)[grep('Effect', names(results))]= effect
  
  rownames(results) <- row_name
  results
  
  
}
