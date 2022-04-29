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
  t_sampling = 100
  Mean1 = 0
  Mean2 = 0.3

##### generate population #####
  population_A.df <- data.frame(
    "ID" = seq(1:n_population) %>% as.character %>%
      str_pad(n_population %>% log10 %>% ceiling, side = "left", "0"),
    "value" = rnorm(n_population,Mean1,1)
  )
  population_B.df <- data.frame(
    "ID" = seq(1:n_population) %>% as.character %>%
      str_pad(n_population %>% log10 %>% ceiling, side = "left", "0"),
    "value" = rnorm(n_population,Mean2,1)
  )

##### Case1: sampling from one population #####
  result_1.lt <- seq(1:t_sampling) %>% lapply(
    function(t_sampling){
      result_1 <- t.test(
        sample(1:n_population, n_sampling) %>% population_A.df[.,] %>% select(value),
        sample(1:n_population, n_sampling) %>% population_A.df[.,] %>% select(value)
      )
      result_1$p.value
    }
  )

  #### original result ####
    # discovery_rate <- ((result_1.lt <= 0.05) %>% sum)/t_sampling
    # cat("DR: ", discovery_rate, "\t")
    
    Case1_OC.lt <-  list( FN = 0,
                          FP = ((result_1.lt <= 0.05) %>% sum),
                          TP = 0,
                          TN = ((result_1.lt > 0.05) %>% sum)
                        )
    Case1_OC.lt$ACC = (Case1_OC.lt$TP+Case1_OC.lt$TN)/t_sampling  #FN+TP
    Case1_OC.lt$Recall <- Case1_OC.lt$TP/(Case1_OC.lt$TP+Case1_OC.lt$FN)
    Case1_OC.lt$SR <- Case1_OC.lt$TN/(Case1_OC.lt$TN+Case1_OC.lt$FP)
    Case1_OC.lt$T1R <- Case1_OC.lt$FP/t_sampling 
    Case1_OC.lt$T2R <- Case1_OC.lt$FN/t_sampling 
  
  #### Bonferroni correction ####
    # Bon_discovery_rate <- ((result_1.lt <= 0.05/t_sampling) %>% sum)/t_sampling
    # cat("Bon_DR: ", Bon_discovery_rate, "\t")
    
    Case1_OC_Bon.lt <-  list( FN = 0,
                              FP = ((result_1.lt <= 0.05/t_sampling) %>% sum),
                              TP = 0,
                              TN = ((result_1.lt > 0.05/t_sampling) %>% sum)
                            )
    Case1_OC_Bon.lt$ACC = (Case1_OC_Bon.lt$TP+Case1_OC_Bon.lt$TN)/t_sampling  #FN+TP
    Case1_OC_Bon.lt$Recall <- Case1_OC_Bon.lt$TP/(Case1_OC_Bon.lt$TP+Case1_OC_Bon.lt$FN)
    Case1_OC_Bon.lt$SR <- Case1_OC_Bon.lt$TN/(Case1_OC_Bon.lt$TN+Case1_OC_Bon.lt$FP)
    Case1_OC_Bon.lt$T1R <- Case1_OC_Bon.lt$FP/t_sampling 
    Case1_OC_Bon.lt$T2R <- Case1_OC_Bon.lt$FN/t_sampling 

  #### BH correction ####
    # result_1.df <- result_1.lt %>% unlist %>% data.frame("pvalue"=.) %>% arrange(pvalue)
    # result_1.df <- result_1.df %>%
    #   mutate("BH" = (row.names(result_1.df) %>% as.numeric())/t_sampling) %>% 
    #   mutate("adj_p" = pvalue/BH)
    # 
    # BH_discovery_rate <- ((result_1.df$adj_p <= 0.05) %>% sum)/t_sampling
    # cat("BH_DR: ", BH_discovery_rate, "\t")
  
    BH_result_1.df <- result_1.lt %>% unlist %>% data.frame("pvalue"=.) %>% arrange(pvalue)
    BH_result_1.df <- BH_result_1.df %>%
      mutate("BH" = (row.names(BH_result_1.df) %>% as.numeric())/t_sampling) %>% 
      mutate("adj_p" = pvalue/BH)

    
    Case1_OC_BH.lt <-  list( FN = 0,
                             FP = ((BH_result_1.df$adj_p <= 0.05) %>% sum),
                             TP = 0,
                             TN = ((BH_result_1.df$adj_p > 0.05) %>% sum)
                           )
    Case1_OC_BH.lt$ACC = (Case1_OC_BH.lt$TP+Case1_OC_BH.lt$TN)/t_sampling  #FN+TP
    Case1_OC_BH.lt$Recall <- Case1_OC_BH.lt$TP/(Case1_OC_BH.lt$TP+Case1_OC_BH.lt$FN)
    Case1_OC_BH.lt$SR <- Case1_OC_BH.lt$TN/(Case1_OC_BH.lt$TN+Case1_OC_BH.lt$FP)
    Case1_OC_BH.lt$T1R <- Case1_OC_BH.lt$FP/t_sampling #TN+FP
    Case1_OC_BH.lt$T2R <- Case1_OC_BH.lt$FN/t_sampling #TN+FP
  
##### Case2: sampling fron two population #####
  result_2.lt <- seq(1:t_sampling) %>% lapply(
    function(t_sampling){
      result_2 <- t.test(
        sample(1:n_population, n_sampling) %>% population_A.df[.,] %>% select(value),
        sample(1:n_population, n_sampling) %>% population_B.df[.,] %>% select(value)
      )
      result_2$p.value
    }
  )

 
  #### original result ####
    # discovery_rate <- ((result_2.lt<=0.05) %>% sum)/t_sampling
    # cat("DR: ", discovery_rate, "\t")
  
    Case2_OC.lt <-  list( FN = ((result_2.lt > 0.05) %>% sum),
                          FP = 0,
                          TP = ((result_2.lt <= 0.05) %>% sum),
                          TN = 0
                         )
    Case2_OC.lt$ACC = (Case2_OC.lt$TP+Case2_OC.lt$TN)/t_sampling  #FN+TP
    Case2_OC.lt$Recall <- Case2_OC.lt$TP/(Case2_OC.lt$TP+Case2_OC.lt$FN)
    Case2_OC.lt$SR <- Case2_OC.lt$TN/(Case2_OC.lt$TN+Case2_OC.lt$FP)
    Case2_OC.lt$T1R <- Case2_OC.lt$FP/t_sampling 
    Case2_OC.lt$T2R <- Case2_OC.lt$FN/t_sampling 
  
  #### Bonferroni correction ####
    # Bon_discovery_rate <- ((result_2.lt <= 0.05/t_sampling) %>% sum)/t_sampling
    # cat("Bon_DR: ", Bon_discovery_rate, "\t")
    
    Case2_OC_Bon.lt <-  list( FN = ((result_2.lt > 0.05/t_sampling) %>% sum),
                              FP = 0,
                              TP = ((result_2.lt <= 0.05/t_sampling) %>% sum),
                              TN = 0
                            )
    Case2_OC_Bon.lt$ACC = (Case2_OC_Bon.lt$TP+Case2_OC_Bon.lt$TN)/t_sampling  #FN+TP
    Case2_OC_Bon.lt$Recall <- Case2_OC_Bon.lt$TP/(Case2_OC_Bon.lt$TP+Case2_OC_Bon.lt$FN)
    Case2_OC_Bon.lt$SR <- Case2_OC_Bon.lt$TN/(Case2_OC_Bon.lt$TN+Case2_OC_Bon.lt$FP)
    Case2_OC_Bon.lt$T1R <- Case2_OC_Bon.lt$FP/t_sampling 
    Case2_OC_Bon.lt$T2R <- Case2_OC_Bon.lt$FN/t_sampling 
    
  #### BH correction ####
    # result_2.df <- result_2.lt %>% unlist %>% data.frame("pvalue"=.) %>% arrange(pvalue)
    # result_2.df <- result_2.df %>%
    #   mutate("BH" = (row.names(result_2.df) %>% as.numeric())/t_sampling) %>% 
    #   mutate("adj_p" = pvalue/BH)
    # 
    # BH_discovery_rate <- ((result_2.df$adj_p <= 0.05) %>% sum)/t_sampling
    # cat("BH_DR: ", BH_discovery_rate, "\t")
      BH_result_2.df <- result_2.lt %>% unlist %>% data.frame("pvalue"=.) %>% arrange(pvalue)
      BH_result_2.df <- BH_result_2.df %>%
        mutate("BH" = (row.names(BH_result_2.df) %>% as.numeric())/t_sampling) %>% 
        mutate("adj_p" = pvalue/BH)
      
      
      Case2_OC_BH.lt <-  list( FN = ((BH_result_2.df$adj_p > 0.05) %>% sum),
                               FP = 0,
                               TP = ((BH_result_2.df$adj_p <= 0.05) %>% sum),
                               TN = 0
      )
      Case2_OC_BH.lt$ACC = (Case2_OC_BH.lt$TP+Case2_OC_BH.lt$TN)/t_sampling  #FN+TP
      Case2_OC_BH.lt$Recall <- Case2_OC_BH.lt$TP/(Case2_OC_BH.lt$TP+Case2_OC_BH.lt$FN)
      Case2_OC_BH.lt$SR <- Case2_OC_BH.lt$TN/(Case2_OC_BH.lt$TN+Case2_OC_BH.lt$FP)
      Case2_OC_BH.lt$T1R <- Case2_OC_BH.lt$FP/t_sampling 
      Case2_OC_BH.lt$T2R <- Case2_OC_BH.lt$FN/t_sampling 
      
##### Visualization #####
  #### ACC Summary ####
    ACC_Sum.df <- data.frame(n_sampling = n_sampling,
                              t_sampling = t_sampling,
                              Mean = Mean2,
                              C1_Ori=Case1_OC.lt$ACC,
                              C1_Bon=Case1_OC_Bon.lt$ACC,
                              C1_BH=Case1_OC_BH.lt$ACC,
                              C2_Ori=Case2_OC.lt$ACC,
                              C2_Bon=Case2_OC_Bon.lt$ACC,
                              C2_BH=Case2_OC_BH.lt$ACC
                              )
    # ACC.p <-  ggplot(ACC_Summ.df, aes(x = gp, y = y)) +geom_point()
      
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
  
  
  
  
  
  