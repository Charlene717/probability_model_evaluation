

##### Presetting ######
  rm(list = ls()) # Clean variable
  memory.limit(150000)

##### Load Packages #####
  library(tidyverse)
  
    
##### Create Real Matrix #####
  Real_NHT <- data.frame(Surger=c(rep("Y",1000),rep("N",1000)),
                         Acne=c(rep("Y",450),rep("N",50),rep("N",450),rep("Y",50),
                                rep("Y",450),rep("N",50),rep("N",450),rep("Y",50)))
  Real_NHF <- data.frame(Surger=c(rep("Y",1000),rep("N",1000)),
                         Acne=c(rep("Y",900),rep("N",100),rep("N",900),rep("Y",100)))
                         
  
##### Test #####
# https://www.cc.ntu.edu.tw/chinese/epaper/0031/20141220_3105.html
  head(iris,5)   
  df <- iris
                         