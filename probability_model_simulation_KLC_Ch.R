##### Presetting ######
  rm(list = ls()) # Clean variable
  memory.limit(150000)
  
##### Load Packages  #####
  library(tidyverse)
  library(caret)
  
##### Function setting  ##### 
  source("Fun_Draw_ConfuMax.R")

##### Parameter setting  #####
  n_population = 1000000
  n_sampling = 100
  t_sampling = 1000

##### generate population #####
  population_A.df <- data.frame(
    "ID" = seq(1:n_population) %>% as.character %>%
      str_pad(n_population %>% log10 %>% ceiling, side = "left", "0"),
    "value" = rnorm(n_population,0,1)
  )
  population_B.df <- data.frame(
    "ID" = seq(1:n_population) %>% as.character %>%
      str_pad(n_population %>% log10 %>% ceiling, side = "left", "0"),
    "value" = rnorm(n_population,0.3,1)
  )

##### sampling from one population #####
  result_1.lt <- seq(1:t_sampling) %>% lapply(
    function(t_sampling){
      result_1 <- t.test(
        sample(1:n_population, n_sampling) %>% population_A.df[.,] %>% select(value),
        sample(1:n_population, n_sampling) %>% population_A.df[.,] %>% select(value)
      )
      result_1$p.value
    }
  )

# original result
  # discovery_rate <- ((result_1.lt <= 0.05) %>% sum)/t_sampling
  # cat("DR: ", discovery_rate, "\t")
  
  FN <- ((result_1.lt <= 0.05) %>% sum)
  FP <- 0
  TP <- ((result_1.lt > 0.05) %>% sum)
  TN <- 0
  Case1_ACC = (TP+TN)/t_sampling  #FN+TP
  Case1_Recall <- TP/(TP+FN)
  Case1_SR <- TN/(TN+FP)
  
  cat("ACC: ", Case1_ACC, "\t")
  
# Bonferroni correction
  # Bon_discovery_rate <- ((result_1.lt <= 0.05/t_sampling) %>% sum)/t_sampling
  # cat("Bon_DR: ", Bon_discovery_rate, "\t")
  Bon_result_1.lt <- result_1.lt/t_sampling

  cat("Bon_ACC: ", Case1_ACC, "\t")
# BH correction
  result_1.df <- result_1.lt %>% unlist %>% data.frame("pvalue"=.) %>% arrange(pvalue)
  result_1.df <- result_1.df %>%
    mutate("BH" = (row.names(result_1.df) %>% as.numeric())/t_sampling) %>% 
    mutate("adj_p" = pvalue/BH)
  
  BH_discovery_rate <- ((result_1.df$adj_p <= 0.05) %>% sum)/t_sampling
  cat("BH_DR: ", BH_discovery_rate, "\t")


##### sampling fron two population #####
  result_2.lt <- seq(1:t_sampling) %>% lapply(
    function(t_sampling){
      result_2 <- t.test(
        sample(1:n_population, n_sampling) %>% population_A.df[.,] %>% select(value),
        sample(1:n_population, n_sampling) %>% population_B.df[.,] %>% select(value)
      )
      result_2$p.value
    }
  )

## Significant changes  
# original result
  # discovery_rate <- ((result_2.lt<=0.05) %>% sum)/t_sampling
  # cat("DR: ", discovery_rate, "\t")

  FN <- 0
  FP <- ((result_2.lt > 0.05) %>% sum)
  TP <- 0
  TN <- ((result_2.lt <= 0.05) %>% sum)
  Case2_ACC = (TP+TN)/t_sampling  #FN+TP
  Case2_Recall <- TP/(TP+FN)
  Case2_SR <- TN/(TN+FP)
  
  cat("ACC: ", Case2_ACC, "\t")
  
  
# Bonferroni correction
  Bon_discovery_rate <- ((result_2.lt <= 0.05/t_sampling) %>% sum)/t_sampling
  cat("Bon_DR: ", Bon_discovery_rate, "\t")

# BH correction
  result_2.df <- result_2.lt %>% unlist %>% data.frame("pvalue"=.) %>% arrange(pvalue)
  result_2.df <- result_2.df %>%
    mutate("BH" = (row.names(result_2.df) %>% as.numeric())/t_sampling) %>% 
    mutate("adj_p" = pvalue/BH)
  
  BH_discovery_rate <- ((result_2.df$adj_p <= 0.05) %>% sum)/t_sampling
  cat("BH_DR: ", BH_discovery_rate, "\t")
  
  
##### Validation #####
  #### Confusion matrix ####
    ## https://www.rdocumentation.org/packages/caret/versions/3.45/topics/confusionMatrix
    library(caret)
    
    numLlvs <- 4
    confusionMatrix(
      factor(sample(rep(letters[1:numLlvs], 200), 50)),
      factor(sample(rep(letters[1:numLlvs], 200), 50)))  
    
    ## https://cran.r-project.org/web/packages/cvms/vignettes/Creating_a_confusion_matrix.html
    ## https://stackoverflow.com/questions/23891140/r-how-to-visualize-confusion-matrix-using-the-caret-package
    ## @Cybernetic
    
  
  
  ## Misclassification rate
  
  
  ## Sensitivity
  
  
  
  ## Specificity
  
  
  
  
  
  