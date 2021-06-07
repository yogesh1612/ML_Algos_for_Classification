# Needs to import ROCR package for ROC curve plotting:
library(ROCR)

plot_roc <- function(model0, df0_test){
  
  model.probs <- predict(model0, df0_test[,-1], type="prob")
  
  pred <- prediction(model.probs[,2], df0_test$y)
  perf <- performance(pred,"tpr","fpr")
  auc_ROCR <- performance(pred, measure = "auc")
  auc <- auc_ROCR@y.values[[1]]; 
  print(auc)  # print this below ROC curve in plots tab
  plot(perf,colorize=TRUE)  # display this in output Plots tab
  legend("bottomright",
         bty = "n",
         horiz = F,
         legend = paste0("AUC Score ",round(auc,3)),
         cex = 1)
  
} # func ends


require(pROC)
plot_roc_multi <- function(model0, df0_test){
  model_preds = predict(model0, df0_test[,-1])
  y_test = as.factor(make.names(df0_test$y))
  a0 = multiclass.roc(y_test, as.numeric(model_preds))
  print(auc(a0))  # print for display
  
  # plotting func. Display the one below
  rs <- a0[['rocs']]; length(rs)
  n1 = length(unique(y_test))
  plot.roc(rs[[1]])
  legend("bottomright",
         bty = "n",
         horiz = F,
         legend = paste0("AUC Score ",round(auc(a0),3)),
         cex = 1)
  sapply(2:n1, function(i) lines.roc(rs[[i]],col=i))
  
} # func ends

#'--- choosing between binary and multiclass ROC --- '
plot_roc_gen <- function(model0, df0_test){
  
  if (length(unique(df0_test$y)) > 2) {
    plot_roc_multi(model0, df0_test)  # works. Whew.
  } else {
    plot_roc(model0, df0_test)  # works easy   
  }
  
} # func ends

runfunc <- function(df0, 
                    kfoldcv_ui = 5, 
                    train_propn_ui = 0.7, 
                    pred_data=NULL, 
                    model_selected_ui = "lg_reg",
                    svm_type = NULL)
{
  
  '--- basic data prep ---'
  require(rsample)
  set.seed(123)
  
  if (is.numeric(df0$y)){
    df0$y = as.factor(paste0('y_', df0$y))
  }else{
      df0$y <- as.factor(df0$y)
    }
  inTrain <- createDataPartition(y = df0$y, p = train_propn_ui, list=FALSE)
  df0_train <-df0[inTrain,]
  df0_test <- df0[-inTrain,]
  
  ' --- common train_control setup for caret ---'
  require(caret)
  train_control <- trainControl(method="repeatedcv", 
                                number=kfoldcv_ui, 
                                repeats=3,
                                classProbs = T,
                                savePredictions = T)
  #, search='random')
  
  
  
  '--- trying logreg now ---'
  if (model_selected_ui == "lg_reg"){ 
    set.seed(1045)
    
    logit.CV <- train(x= df0_train[,-1] , y= df0_train[,1], 
                      method = 'glm',
                      family = 'binomial',
                      trControl = train_control)
    
    return(list(logit.CV,df0_train,df0_test))
 
  } # model ends
  
  '--- below all are for SVM ---'
  if (model_selected_ui=="svm"){
    set.seed(1045)
    
    if (svm_type == "SVM_linear_fixed"){ 
      set.seed(1045)
      
      svm2 <- train(y ~., data = df0_train, method = "svmLinear", 
                    trControl = train_control,  preProcess = c("center","scale")) #7s
      
      return(list(svm2,df0_train,df0_test))
    
    } # MODEL ENDS
    
    
    if (svm_type == "SVM_linear_grid"){
      set.seed(1045)
      
      svm3 <- train(y ~., data = df0_train, method = "svmLinear", 
                    trControl = train_control,  preProcess = c("center","scale"), 
                    tuneGrid = expand.grid(C = seq(0, 3, length = 25)))  # # 6s
      
      return(list(svm3,df0_train,df0_test))
    }
    
    
    ## non-linear SVM using RBFs
    if (svm_type == "SVM_RBF"){
      set.seed(1045)
      
      svm3a <- train(y ~., data = df0_train, method = "svmRadial", 
                     trControl = train_control, preProcess = c("center","scale"), 
                     tuneLength = 10)
      
      return(list(svm3a,df0_train,df0_test))
    } # model ends
    
    ## non-linear SVM using polynomial bases
    if (svm_type == "SVM_polynomial"){
      set.seed(1045)
      
      svm4 <- train(y ~., data = df0_train, method = "svmPoly", 
                    trControl = train_control, preProcess = c("center","scale"), 
                    tuneLength = 4)
      
      return(list(svm4,df0_train,df0_test))
    } # MODEL ENDS
    
  } #---SVM ends here
  
  if (model_selected_ui == "nb"){ 
    set.seed(1045)
    
    # trainControl in caret for cross-validn in classifn
    nb_trControl <- trainControl(method = "cv",
                                 number = kfoldcv_ui,
                                 # repeats = 3,
                                 classProbs = TRUE,
                                 summaryFunction = multiClassSummary) 
    
    
    # run knn in caret now
    system.time({
      set.seed(1045)
      suppressWarnings(
      nb_fit <- train(y ~ .,
                      data = df0_train,
                      method = 'nb',
                      trControl = nb_trControl,
                      #preProc = c("center", "scale"),
                      metric = 'ROC')
      )
     })
  return(list(nb_fit,df0_train,df0_test))
} # func ends

  if (model_selected_ui == "nn"){ 
    set.seed(1045)
    
    # trainControl in caret for cross-validn in classifn
    nnet_trControl <- trainControl(method = "cv",
                                   number = kfoldcv_ui,
                                   # repeats = 3,
                                   classProbs = TRUE,
                                   summaryFunction = multiClassSummary) 
    
    
    # run knn in caret now
    set.seed(123)
    system.time({
      set.seed(1045)
      
      nnet_fit <- train(y ~ .,
                        data = df0_train,
                        method = 'nnet',
                        trControl = nnet_trControl,
                        #preProc = c("center", "scale"),
                        metric = 'ROC',
                        tuneGrid=expand.grid(size=c(10), decay=c(0.1)))  
    }) # 26 secs!
    return(list(nnet_fit,df0_train,df0_test))
  }

}

pca_plot <- function(y,X){
  
  y = y; X = X
  
  if (is.numeric(y)){y = as.character(paste0('y_', y))}
  X_num <- X %>% dplyr::select(where(is.numeric))
  #a0 = apply(X, 2, function(x) {is.numeric(x)}) %>% which(.) %>% as.numeric(); a0
  a1 = princomp(X_num, cor=TRUE)$scores[,1:2]
  a2 = data.frame(y=y, x1=a1[,1], x2=a1[,2])
  
  p <- ggplot(data=a2, aes(x=x1, y=x2, colour = factor(y))) + 
    geom_point(size = 4, shape = 19, alpha = 0.6) + 
    xlab("PCA compt1") + ylab("PCA compt 2")
  
  plot(p)  } # func ends

