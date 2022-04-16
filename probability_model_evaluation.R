## R ????????????????????????
## https://rpubs.com/ivan0628/probability_model_evaluation

##### Presetting ######
  rm(list = ls()) # Clean variable
  memory.limit(150000)

#### Load Packages ####
    
  #install.packages('ROCR')
  #require(ROCR)
  require(ROCR)
  

#### confusion matrix ####  
  #???ROCR???????????????????????????
  data("ROCR.simple")
  df <- data.frame(ROCR.simple)
  df$predclass <- ifelse(df$predictions>0.5, 1, 0) #???0.5??????????????? 
  print(cf <- table(df[,c("predclass","labels")]))
  
  
  tp <- cf[2, 2]
  tn <- cf[1, 1]
  fp <- cf[2, 1]
  fn <- cf[1, 2]
  
  accuracy <- (tp + tn)/(tp + tn + fp + fn); accuracy
  specificity <- tn/(tn + fp); specificity
  
  
  #install.packages('caret')
  #require(caret)
  
  require(caret)
  confusionMatrix(cf, positive = "1")

##### ROC #####
  #install.packages('ROCR')
  require(ROCR)
  require(kableExtra)
  data(ROCR.simple)
  kable(as.data.frame(ROCR.simple)[1:10, ]) %>% kable_styling() %>% column_spec(1:2,width = "20em") #?????????????????????20?????????
  
  pred <- ROCR::prediction(ROCR.simple$predictions, ROCR.simple$labels) 
  perf <- ROCR::performance(pred,"tpr","fpr")
  #???AUC???
  auc <- performance(pred,'auc') %>% unlist() %>% slot("y.values") %>% unlist()
  #??????
  plot(perf,colorize=TRUE,main=paste0("ROC Curve , AUC = ",round(auc,2)));abline(0,1);grid()

#### KS ?????? ####
  perf <- ROCR::performance(pred,"tpr","fpr")
  FPR=attr(perf,'x.values')[[1]]
  TPR=attr(perf,'y.values')[[1]]
  Cutoff=attr(perf,'alpha.values')[[1]]
  ks_df <- data.frame(FPR,TPR,Cutoff,KS=TPR-FPR)
  
  ks_max <- max(ks_df$KS)
  plotKS <- ggplot(ks_df)+
    geom_line(aes(Cutoff,TPR),colour="red2",size=1.2)+
    geom_line(aes(Cutoff,FPR),colour="blue3",size=1.2)+
    geom_line(aes(Cutoff,TPR-FPR),colour="forestgreen",size=1.2)+
    geom_vline(xintercept=ks_df[ks_df$KS==ks_max,"Cutoff"],linetype=2,colour="gray",size=0.6)+
    geom_hline(yintercept=ks_df[ks_df$KS==ks_max,"TPR"],linetype=2,colour="red2",size=0.6)+
    geom_hline(yintercept=ks_df[ks_df$KS==ks_max,"FPR"],linetype=2,colour="blue3",size=0.6)+
    geom_hline(yintercept=ks_df[ks_df$KS==ks_max,"KS"],linetype=2,colour="forestgreen",size=0.6)+
    geom_text(aes(x = 0.25, y = 1.0, label = "TPR"),colour="red2") + 
    geom_text(aes(x = 0.125, y = 0.60, label = "FPR"), colour = "blue3") + 
    xlab("Cutoff")+
    ylab("TPR & FRP")+
    ggtitle(label="KS - Chart")+
    theme_bw()+
    theme(plot.title=element_text(colour="gray24",size=12,face="bold"),
          plot.background = element_rect(fill = "gray90"),
          axis.title=element_text(size=10),
          axis.text=element_text(colour="gray35"),
          legend.position = "right")
  plotKS


  ks.plot = function(prediction,label,n=100){
    df <- data.frame(prediction,label) %>% arrange(desc(prediction)) 
    total_positive <- sum(df$label)
    total_negative <- nrow(df)-total_positive
    # n?????????????????????0-1????????????????????????
    # ???????????????100??????????????????100?????????????????????????????????????????????
    if(n > nrow(df)){n <- nrow(df)}
    df$rownum <- 1:nrow(df)
    qus <- quantile(1:nrow(df),probs=seq(0,1,1/n)) #???????????????????????????n???
    success_prop <- failure_prop <- NULL
    out <- mapply(function(i){
      sub_df <- df[df$rownum < ifelse(i==n,qus[i+1]+0.001,qus[i+1]),] #??????????????????
      success_prop <<-c(success_prop,sum(sub_df$label==1)) #?????????1???????????????
      failure_prop <<-c(failure_prop,sum(sub_df$label==0)) #?????????0??????????????? 
    },1:n)
    success_prop <- success_prop/total_positive
    failure_prop <- failure_prop/total_negative
    ks_df <- data.frame(rownum=1:n,success_prop,failure_prop,KS=success_prop-failure_prop)
    ks_max <- max(ks_df$KS)
    # ????????????
    plotKS <- ggplot()+
      geom_line(aes(ks_df$rownum,ks_df$success_prop),colour="red2",size=1.2)+
      geom_line(aes(ks_df$rownum,ks_df$failure_prop),colour="blue3",size=1.2)+
      geom_line(aes(ks_df$rownum,ks_df$KS),colour="forestgreen",size=1.2)+
      geom_vline(xintercept=ks_df[ks_df$KS==ks_max,"rownum"],linetype=2,colour="gray",size=0.6)+
      geom_hline(yintercept=ks_df[ks_df$KS==ks_max,"success_prop"],linetype=2,colour="red2",size=0.6)+
      geom_hline(yintercept=ks_df[ks_df$KS==ks_max,"failure_prop"],linetype=2,colour="blue3",size=0.6)+
      geom_hline(yintercept=ks_df[ks_df$KS==ks_max,"KS"],linetype=2,colour="forestgreen",size=0.6)+
      annotate("text", x = 10, y = 1.05, label=paste("KS=", round(ks_max, 4)), size=4,alpha=0.8)+
      geom_text(aes(x = 75, y = 1.0, label = "TPR"),colour="red2") + 
      geom_text(aes(x = 80, y = 0.6, label = "FPR"), colour = "blue3") + 
      xlab("% allocated to class positive")+
      ylab("Cumulative proportion")+
      ggtitle(label="KS - Chart")+
      theme_bw()+
      theme(plot.title=element_text(colour="gray24",size=12,face="bold"),
            plot.background = element_rect(fill = "gray90"),
            axis.title=element_text(size=10),
            axis.text=element_text(colour="gray35"))
    return(list(plotKS=plotKS,ks_max=ks_max))
  }
  
  ks.plot(ROCR.simple$predictions,ROCR.simple$labels,100)

#### Gain ??????????????? ####
  pred <- ROCR::prediction(ROCR.simple$predictions, ROCR.simple$labels) 
  gain <- ROCR::performance(pred, "tpr", "rpp")
  plot(gain, main = "Gain Chart",col = "red2", lwd = 3)
  
  lift <- ROCR::performance(pred,"lift","rpp")
  plot(lift, main = "Lift Chart",col = "red2", lwd = 3)
  
  #### Response ??????????????? ####
  pred <- ROCR::prediction(ROCR.simple$predictions, ROCR.simple$labels) 
  response <- ROCR::performance(pred,"ppv","rpp")
  plot(response, main = "Response Chart",col = "red2", lwd = 3)
  
