
shinyUI(fluidPage(
  
  title = "ML Algos for Classification",
  titlePanel(title=div(img(src="logo.png",align='right'),"ML Algos for Classification")),
  sidebarPanel(
    source("scripts//uiInput.R",local = TRUE)[[1]], 
    conditionalPanel(condition = "input.tabselected==3",
                     
                     ),
    conditionalPanel(condition="input.tabselected==3",
                     uiOutput("y_ui"),
                     uiOutput("x_ui"),
                     # radioButtons("task","Select task",
                     #              choices = c("Classification" = 'clf',
                     #                           "Regression" = "reg")),
                     radioButtons("model_sel","Select Model",
                                  choices = c("Logistic Regression" = "lg_reg",
                                              "Naive Bayes" = "nb",
                                              "SVM" = "svm",
                                              "Neural Networks" = "nn")),
                     uiOutput("svm_type_ui"),
                     sliderInput("tr_per",
                                 label = "Percentage of training data",
                                 min = 0,
                                 max = 1,
                                 value = 0.7,
                                 step = 0.05),
                     sliderInput("kfold",
                                 label = "Number of CV folds",
                                 min = 1,
                                 max = 10,
                                 value = 3,
                                 step = 2),
                    actionButton("apply","Train model")
    )
    
  ),
  mainPanel(
    # recommend review the syntax for tabsetPanel() & tabPanel() for better understanding
    # id argument is important in the tabsetPanel()
    # value argument is important in the tabPanle()
    tabsetPanel(
      tabPanel("Overview & Example Dataset", value=1, 
               includeMarkdown("overview.md")
      ),
      tabPanel("Data Summary", value=3,
               DT::dataTableOutput("samp"),
               hr(),
               h4("Data Structure"),
               verbatimTextOutput("data_str"),
               h4("Missingness Map"),
               plotOutput("miss_plot")
               
      ),
      tabPanel("Model Results", value=3,
               h4("Model Summary"),
               helpText("Training may take a while, upto a minute"),
               verbatimTextOutput("mod_sum"),
               p("Model Result"),
               verbatimTextOutput("mod_res"),
               hr(),
               # h4("Confusion Matrix (Train Set)"),
               # plotOutput('conf_train_plot'),
               #HTML('<button data-toggle="collapse" data-target="#demo">Detailed Result</button>'),
              # tags$div(id="demo",class="collapse",),
             #  verbatimTextOutput("conf_train"),
               #hr(),
           
               #verbatimTextOutput("conf_test")
               #tags$div(id="demo1",class="collapse",)
               
      ),
      tabPanel("Plots",value=3,
               h4('PCA plot'),
               plotOutput("pca_plot"),
               h4("Confuison Matrix (Test Set)"),
               # HTML('<button data-toggle="collapse" data-target="#demo1">Detailed Result</button>'),
               plotOutput('conf_test_plot')
              # h4("Error Rate Plot"),
              # plotOutput("err_rate"),
              # h4("ROC-AUC Curve"),
              # plotOutput("roc")
      ),
   
      tabPanel("Prediction Output",value=3,
               helpText("Note: Please upload test data with same features in train dataset"),
               DT::dataTableOutput("test_op"),
               downloadButton("download_pred")
               
      ),
      id = "tabselected"
    )
  )
))

