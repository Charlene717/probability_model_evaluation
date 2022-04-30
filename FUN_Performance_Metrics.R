Performance_Metrics <- function(FN,FP,TP,TN) {
  PM_OC <- list()
  t_sampling <- FN+FP+TP+TN
  PM_OC.lt$ACC = (TP+TN)/t_sampling  
  PM_OC.lt$Recall <- TP/(TP+FN)
  PM_OC.lt$SR <- TN/(TN+FP)
  PM_OC.lt$T1R <- FP/t_sampling 
  PM_OC.lt$T2R <- FN/t_sampling
  
  
 return() 
}