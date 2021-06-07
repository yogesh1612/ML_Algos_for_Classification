options(warn=-1)

server <- function(input, output,session) {
  
  tr_data <-  reactive({
    req(input$tr_data$datapath)
    df <- read.csv(input$tr_data$datapath,stringsAsFactors = FALSE)
    return(df)
  })
  
  test_data <-  reactive({
    req(input$test_data$datapath)
    df <- read.csv(input$test_data$datapath,stringsAsFactors = FALSE)
    return(df)
  })
  
  tr_cols <- reactive({
    req(input$tr_data$datapath)
    return(colnames(tr_data()))
    })
  

  #----Tab-2 Data Summary----#
  
  output$samp <- DT::renderDataTable({
    req(input$tr_data$datapath)
    DT::datatable(tr_data(),
                  #filter = "top"
                  options = list(lengthMenu = list(c(10,25,50,-1),c("5","25","50","All")),
                                autoWidth = TRUE),
                  caption = "Table 1: Sample of Data"
                  )
  })
  
  output$data_str <- renderPrint({
    str(tr_data())
  })
  
  output$miss_plot <- renderPlot({
    req(input$tr_data$datapath)
    Amelia::missmap(tr_data())
  })
  
  
  #-----------------------#
  output$y_ui <- renderUI({
    req(input$tr_data$datapath)
    selectInput(inputId = 'sel_y',label = "Select Y (Target Variable)",choices = tr_cols(),multiple = FALSE)
  })
  
  x_col <- reactive({
    req(input$tr_data$datapath)
    x <- match(input$sel_y,tr_cols())
    y_col <- tr_cols()[-x]
    return(y_col)
  })
  
  output$x_ui <- renderUI({
    req(input$tr_data$datapath)
    selectInput(inputId = "sel_x",label="Select X (Features)",choices = x_col(),multiple = TRUE,selected = x_col())
  })
  
  
  output$pca_plot <- renderPlot({
    y <- tr_data()[,input$sel_y]
    X <- tr_data()[,input$sel_x]
    X <- select_if(X,is.numeric)
    pca_plot(y,X)
  })
  
  output$svm_type_ui <- renderUI({
    if(input$model_sel=="svm"){
      
      selectInput('svm_type',label = "Select Model",
                        choices = c("Linear SVM" = "SVM_linear_fixed",
                                    "Linear SVM Grid Search" = "SVM_linear_grid",
                                    "Nonlinear SVM with Radial Basis" = "SVM_RBF",
                                    "Nonlinear SVM with Polynomial Basis" = "SVM_polynomial")
                  )
    }else{
        return(NULL)
      }
   
  })
  
  model <- eventReactive(input$apply, {
    y <- tr_data()[,input$sel_y]
    X <- tr_data()[,input$sel_x]
    df0 <- data.frame(y,X)
    #df0 
   withProgress(message = 'Training in progress. Please wait ...',
   mod_list <- runfunc(df0 = df0,
                 kfoldcv_ui = input$kfold,
                 train_propn_ui = input$tr_per,
                 pred_data = NULL,
                 model_selected_ui = input$model_sel,
                 svm_type = input$svm_type)
    )
   return(mod_list)
    # 
    #            
    # p1 <- predict(rf, train_data)
    # if (input$task == 'clf') {
    #   train_conf = caret::confusionMatrix(p1, train_data[,1])  
    #   #a1$table # print this as html table
    #   #print(a1)  # as raw text below the html tbl
    # }
    # p2 <- predict(rf, test_data)
    # if (input$task == 'clf') {
    #   test_conf = caret::confusionMatrix(p2, test_data[,1])  
    #   #a1$table # print this as html table
    #   #print(a1)  # as raw text below the html tbl
    # }
    # output$roc <- renderPlot({plot_roc(rf,test_data$y,test_data[,-1])})
    # #roc.plot <- plot_roc(rf,test_data$y,test_data[,-1])
    # list(rf,train_conf,test_conf)
    
  })
  
  
  output$t1 <- renderUI({
    if(input$model_sel=="lg_reg"){
      checkboxInput('sho_log',"Show coefficients",value = T)
    }else{
      NULL
    }
  })
  
  output$t2 <- renderUI({
    if(input$sho_log==T){
      DT::dataTableOutput("lg_reg_tb")
    }else{
      NULL
    }
  })
  
  
  output$lg_reg_tb <- renderDataTable({
    req(model())
    summ_tb <-  as.data.frame(summary(model()[[1]])$coefficients %>% round(., 3))
    summ_tb
  })
  
  output$mod_res <- renderPrint({
    req(model())
    model()[[1]]$results
  })
  
  output$mod_sum <- renderPrint({
    req(model())
    if(input$model_sel=='lg_reg'){
      summary(model()[[1]])
    }
    else{
      model()[[1]]
    }
    
  })

  # output$conf_train_plot <- renderPlot({
  #   fourfoldplot(data()[[2]]$table,
  #                color = c("#CC6666", "#99CC99"),
  #                conf.level = 0,
  #                main="")
  # })
  # 
  train_predict <- reactive(
    {
      req(model())
      pred <- predict(model()[[1]],model()[[2]][,input$sel_x])
      pred
    }
  )
  test_predict <- reactive(
    {
      req(model())
      pred <- predict(model()[[1]],model()[[3]][,input$sel_x])
      pred
    }
  )
  output$conf_train <- renderPrint({
    #cnf <- confusionMatrix(train_predict(),model()[[2]]$y)
    cnf <- confusionMatrix(model()[[1]])
   # cat("---Confusion Matrix (Training Data)---")
    cnf

    #cat("\nAccuracy on training data is ", cnf[[3]][1])
  })
  
  output$conf_test <- renderPrint({
    cnf <- confusionMatrix(test_predict(),model()[[3]]$y)
    cnf
  })


output$tar_dis <- renderPrint({
  req(tr_data())
  round(prop.table(table(tr_data()[,input$sel_y])),3)
})

  #----RF Plot output tab ------#
  # output$err_rate <- renderPlot({
  #   req(tr_data())
  #   plot(data()[[1]],main="Error Rate")
  # })
  # 
  output$roc <- renderPlot({
    plot_roc_gen(model()[[1]],model()[[3]])
  })
  # 
  # #-----Var Imp Plot ----#
  # 
  # output$n_tree <- renderPlot({
  #   hist(treesize(data()[[1]]),
  #        main = "No. of Nodes for the Trees",
  #        col = "green",xlab="Tree Size")
  # })
  # 
  # output$var_imp <- renderPlot({
  #   varImpPlot(data()[[1]],
  #              sort = T,
  #              n.var = 10,
  #              main = "Top 10 - Variable Importance")
  # })
  # 
  # output$var_imp_tb <- renderDataTable({
  #   imp_df = data.frame("Features" = rownames(importance(data()[[1]])),
  #                       "MeanDecreaseGini"=round(importance(data()[[1]]),2))
  #   a0 = sort(imp_df$MeanDecreaseGini, decreasing = TRUE, index.return=TRUE)$ix
  #   rownames(imp_df) = NULL
  #  # names(imp_df) <- c("Features","MeanDecreaseGini")
  #   imp_df[a0,]
  #  
  # })
  
  # Prediction Tab----#
  out_pred_df <- reactive({
    req(test_data())
    pred_data <- test_data()[,input$sel_x]
    p3 = predict(model()[[1]], pred_data)
    out_pred_df = data.frame("prediction" = p3, pred_data)
    })# downloadable file. })
  # 
  output$test_op <- DT::renderDataTable({
       head(out_pred_df(), 25) # display 10 rows of this as HTML tbl
  })
  # 
  output$download_pred <- downloadHandler(
    filename = function() { "predictions.csv" },
    content = function(file) {
      write.csv(out_pred_df(), file,row.names=FALSE)
    }
  )
  # 
}