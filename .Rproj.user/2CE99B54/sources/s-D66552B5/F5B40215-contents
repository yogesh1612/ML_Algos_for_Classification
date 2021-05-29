# Needs to import ROCR package for ROC curve plotting:
library(ROCR)

plot_roc <- function(rf, test_y, test_X){  # test_y = test$y
  
  prediction_for_roc_curve <- predict(rf, test_X, type="prob")
  
  pretty_colours <- c("#F8766D","#00BA38","#619CFF", "orange", "purple", "azure3")
  
  classes <- levels(test_y)
  
  for (i in 1:length(classes)){
    
    true_values <- ifelse(test_y == classes[i], 1, 0) # Define class[i] membership
    
    pred <- prediction(prediction_for_roc_curve[,i], true_values) # Assess classifier perf for class[i]
    
    perf <- performance(pred, "tpr", "fpr")
    if (i==1)
    {
      plot(perf, main="ROC Curve", col=pretty_colours[i]) 
    }
    else
    {
      plot(perf, main="ROC Curve", col=pretty_colours[i], add=TRUE) 
    }
    
    # abline(a=0, b=1, col="black")
    auc.perf <- performance(pred, measure = "auc") # Calc AUC and print on screen
    print(auc.perf@y.values)
  } # i loop ends
  
} # func ends

# test-drive
# plot_roc(rf, test$y, test[,-1])


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
  
  if (is.numeric(df0$y)){df0$y = as.factor(paste0('y_', df0$y))}
  inTrain <- createDataPartition(y = df0$y, p = train_propn_ui, list=FALSE)
  df0_train <-df0[inTrain,]
  df0_test <- df0[-inTrain,]
  
  ' --- common train_control setup for caret ---'
  require(caret)
  train_control <- trainControl(method="repeatedcv", 
                                number=kfoldcv_ui, 
                                repeats=3)
  #, search='random')
  
  
  
  '--- trying logreg now ---'
  if (model_selected_ui == "lg_reg"){ 
    logit.CV <- train(x= df0_train[,-1] , y= df0_train[,1], 
                      method = 'glm',
                      family = 'binomial',
                      trControl = train_control)
    
    return(list(logit.CV,df0_test))
 
  } # model ends
  
  '--- below all are for SVM ---'
  if (model_selected_ui=="svm"){
    if (svm_type == "SVM_linear_fixed"){ 
      svm2 <- train(y ~., data = df0_train, method = "svmLinear", 
                    trControl = train_control,  preProcess = c("center","scale")) #7s
      
      return(list(svm2,df0_test))
    
    } # MODEL ENDS
    
    
    if (svm_type == "SVM_linear_grid"){
      
      svm3 <- train(y ~., data = df0_train, method = "svmLinear", 
                    trControl = train_control,  preProcess = c("center","scale"), 
                    tuneGrid = expand.grid(C = seq(0, 3, length = 25)))  # # 6s
      
      return(list(svm3,df0_test))
    }
    
    
    ## non-linear SVM using RBFs
    if (svm_type == "SVM_RBF"){
      svm3a <- train(y ~., data = df0_train, method = "svmRadial", 
                     trControl = train_control, preProcess = c("center","scale"), 
                     tuneLength = 10)
      
      return(list(svm3a,df0_test))
    } # model ends
    
    ## non-linear SVM using polynomial bases
    if (svm_type == "SVM_polynomial"){
      
      svm4 <- train(y ~., data = df0_train, method = "svmPoly", 
                    trControl = train_control, preProcess = c("center","scale"), 
                    tuneLength = 4)
      
      return(list(svm4,df0_test))
    } # MODEL ENDS
    
  } #---SVM ends here
  
  if (model_selected_ui == "nb"){ 
    # trainControl in caret for cross-validn in classifn
    nb_trControl <- trainControl(method = "cv",
                                 number = kfoldcv_ui,
                                 # repeats = 3,
                                 classProbs = TRUE,
                                 summaryFunction = twoClassSummary) 
    
    
    # run knn in caret now
    set.seed(123)
    system.time({
      
      nb_fit <- train(y ~ .,
                      data = df0_train,
                      method = 'nb',
                      trControl = nb_trControl,
                      #preProc = c("center", "scale"),
                      metric = 'ROC')
     })
  return(list(nb_fit,df0_test))
} # func ends

  if (model_selected_ui == "nn"){ 
    # trainControl in caret for cross-validn in classifn
    nnet_trControl <- trainControl(method = "cv",
                                   number = kfoldcv_ui,
                                   # repeats = 3,
                                   classProbs = TRUE,
                                   summaryFunction = twoClassSummary) 
    
    
    # run knn in caret now
    set.seed(123)
    system.time({
      nnet_fit <- train(y ~ .,
                        data = df0_train,
                        method = 'nnet',
                        trControl = nnet_trControl,
                        #preProc = c("center", "scale"),
                        metric = 'ROC',
                        tuneGrid=expand.grid(size=c(10), decay=c(0.1)))  
    }) # 26 secs!
    return(list(nnet_fit,df0_test))
  }

}

pca_plot <- function(y,X){
  
  y = y; X = X
  
  if (is.numeric(y)){y = as.character(paste0('y_', y))}
  a0 = apply(X, 2, function(x) {is.numeric(x)}) %>% which(.) %>% as.numeric(); a0
  a1 = princomp(X[,a0], cor=TRUE)$scores[,1:2]
  a2 = data.frame(y=y, x1=a1[,1], x2=a1[,2])
  
  p <- ggplot(data=a2, aes(x=x1, y=x2, colour = factor(y))) + 
    geom_point(size = 4, shape = 19, alpha = 0.6) + 
    xlab("PCA compt1") + ylab("PCA compt 2")
  
  plot(p)  } # func ends

