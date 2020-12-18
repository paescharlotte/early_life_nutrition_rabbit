##code from CAUQUIL L.
report_lme <- function(lme_result){
  list_model <- attributes(lme_result$pval[[1]])$row.names
  tmp <- matrix(0, ncol=length(list_model)+2, nrow=length(lme_result$names) ,dimnames=list(lme_result$names, c(paste(list_model[(seq(list_model))], "(p-val)"),"W", "p-val (W)")))
  for(i in seq(length(lme_result$pval))){
    for (j in seq(length(list_model))){
      tmp[i,j] <- lme_result$pval[[i]][[3]][j] 
    }
    tmp[i,j+1] <- lme_result$shap[[i]]$statistic
    tmp[i,j+2] <- lme_result$shap[[i]]$p.value
  }
  return(tmp)
  assign(paste0(lme_result,"report_lme"),as.data.frame(tmp), envir = parent.frame())#.GlobalEnv)
}
