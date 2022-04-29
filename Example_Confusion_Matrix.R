##### Presetting ######
  rm(list = ls()) # Clean variable
  memory.limit(150000)

##### Load Packages  #####     
  library(caret)
  
##### Function setting  ##### 
  source("Fun_Draw_ConfuMax.R")
  
##### construct the evaluation dataset #####
  set.seed(717)
  true_class <- factor(sample(paste0("Class", 1:2), size = 1000, 
                              prob = c(.2, .8), replace = TRUE))
  true_class <- sort(true_class)
  class1_probs <- rbeta(sum(true_class == "Class1"), 4, 1)
  class2_probs <- rbeta(sum(true_class == "Class2"), 1, 2.5)
  test_set <- data.frame(obs = true_class,
                         Class1 = c(class1_probs, class2_probs))
  test_set$Class2 <- 1 - test_set$Class1
  test_set$pred <- factor(ifelse(test_set$Class1 >= .5, "Class1", "Class2"))

##### calculate the confusion matrix #####
  library(caret)
  cm <- confusionMatrix(data = test_set$pred, reference = test_set$obs)    
  
##### Draw Confusion Matrix #####
  source("Fun_Draw_ConfuMax.R")
  draw_confusion_matrix(cm)
  