# Define UI for dataset viewer application
shinyUI(pageWithSidebar(
  # Application title
  headerPanel("Easy Feedforward Neural Networks (beta version)"),
  
  sidebarPanel(p(HTML("Train and analyze feedforward neural networks 
                      (aka multilayer perceptrons) for regression and 
                      classification tasks.")),
               selectInput("fit",
                           "Active neural network:",
                           choices= "(No trained networks)"),
               verbatimTextOutput("summary")
  ),

  mainPanel(
    tabsetPanel(

      tabPanel("Preview Data",
               h2("Data importation"),
               p(HTML("To run the application, import your data set using the 
                import button on the left panel. Your data must be supplied 
                in the form of a text/csv or excel file. If the importation is  
                done properly, a preview of the data is displayed below as a 
                table. When this is done, you can proceed to the next step: 
                Train Net")),
               selectInput("filetype", "Type of file", 
                           list("CSV or Text"= "csv",
                                "Excel"= "xls")),
               fileInput('file1', "Choose File:"),
               checkboxInput('header', ' Header?', TRUE),
               conditionalPanel("input.filetype == 'csv'",
                                checkboxInput('rownames', ' Row names?', FALSE),
                                selectInput('sep', 'Separator:', 
                                            c(Comma=',', Semicolon=';', 
                                              Tab='\t', Space=' '),
                                            'Comma'),
                                selectInput('quote', 'Quote:',  
                                            c(None='', 'Double Quote'='"', 
                                              'Single Quote'="'"), 
                                            'Double Quote'),
                                selectInput('dec', 'Decimal mark', 
                                            c(Period='.', Comma=','), 
                                            'Period')
                                ),

               numericInput('nrow.preview', 'Number of rows in the preview:', 20),
               numericInput('ncol.preview', 'Number of columns in the preview:', 10),
               helpText("Note: while the preview will show only the specified number 
                         of observations, the map will be based on the full dataset."),
               tableOutput("view")
      ),

      tabPanel("Train Net",
               verbatimTextOutput("trainMessage"),
               
               h4("Training options"),
               selectInput("algo", "Type of network :", 
                           choices= list("Multilayer Perceptron (rprop)"= "rprop", 
                                         "Extreme Learning Machine"= "elm", 
                                         "Multilayer Perceptron (nnet)"= "nnet")),
               actionButton("trainbutton","Train"),
               conditionalPanel("input.algo == 'rprop'", 
                                selectInput("nhidlay", "Number of hidden layers:", 
                                            choices= 1:4)),
               numericInput("nhid1", 
                            "Number of neurons in hidden layer 1:",
                            5, min= 1),
               conditionalPanel("input.algo == 'rprop'", 
                                conditionalPanel("input.nhidlay >= 2", 
                                                 numericInput("nhid2", 
                                                              "Number of neurons in hidden layer 2:", 
                                                              5, min= 1)),
                                conditionalPanel("input.nhidlay >= 3", 
                                                 numericInput("nhid3", 
                                                              "Number of neurons in hidden layer 3:", 
                                                              5, min= 1)),
                                conditionalPanel("input.nhidlay >= 4", 
                                                 numericInput("nhid4", 
                                                              "Number of neurons in hidden layer 4:", 
                                                              5, min= 1))
               ),
               conditionalPanel("input.algo != 'elm'",
                                numericInput("maxit", "Max. iterations:", 100),
                                numericInput("regul", 
                                             "Weight decay parameter:", 
                                             0, min= 0, step= .1)
               ),
               selectInput("activout", "Output neurons activation function:", 
                           choices= c("identity", "softmax")),
               br(),
               h4("Select input and output variables"),
               verbatimTextOutput("varMessage"),
               uiOutput("varchoice"),
               
               br(),
               h4("Advanced options"),
               numericInput("ncommittee", "Number of neural networks in the
                            committee:", value= 1, min= 1),
               uiOutput("ntrain"),
               selectInput("activhid", "Hidden neurons activation function:", 
                           choices= c("htan", "logistic")),
               numericInput("randseed", 
                            "Random seed for reproducible results:",
                            sample(1:1e5, size= 1))
      ),
      
      tabPanel("Diagnostics",
               selectInput("diagplottype", "Type of plot:",
                           choices= list("Training error"= "trainerr",
                                         "Actual vs Fitted"= "actfit",
                                         "Error distribution"= "errdistr",
                                         "Residual vs Fitted"= "residfit",
                                         "Residual vs Predictor"= "residinput")
               ),
               conditionalPanel("input.diagplottype == 'actfit' ||
                                input.diagplottype == 'errdistr' ||
                                input.diagplottype == 'residfit' ||
                                input.diagplottype == 'residinput'", 
                                selectInput("diagsample", "Observations sample:",
                                            choices= c("test", "training", "whole")),
                                selectInput("diagvarchoiceout", "Output variable", 
                                            choices= "(First train)")
               ),
               conditionalPanel("input.diagplottype == 'residinput'", 
                                selectInput("diagvarchoicein", "Input variable", 
                                            choices= "(First train)")
               ),
               plotOutput("diagplot")
               ),
      
      tabPanel("Prediction",
               downloadButton("preddownload", "Download predictions"),
               selectInput("predsample", "Observations sample:",
                           choices= c("whole", "test", "training")),
               selectInput("predvarchoicein", "Input variable", 
                           choices= "(First train)"),
               selectInput("predvarchoiceout", "Output variable", 
                           choices= "(First train)"),
               checkboxInput("predshowobs", "Show actual observations", FALSE),
               checkboxInput("predsmooth", "Show smooth lowess curve", TRUE),
               conditionalPanel("input.predsmooth == 1",
                                numericInput("predLowessAlpha", 
                                             "Lowess smoothing parameter:",
                                             min= 0, max= 1, 
                                             value= .3, step= .1)),
               plotOutput("predplot")
               ),
      
      tabPanel("Partial derivatives",
               actionButton("derbutton", "Compute"),
               downloadButton("derdownload", "Download partial derivatives"),
               br(), br(),
               verbatimTextOutput("dertext"),
               selectInput("dersample", "Observations sample:",
                           choices= c("whole", "test", "training")),
               selectInput("dervarchoicein", "Input variable", 
                           choices= "(First train)"),
               selectInput("dervarchoiceout", "Output variable", 
                           choices= "(First train)"),
               checkboxInput("dersmooth", "Show smooth lowess curve", TRUE),
               conditionalPanel("input.dersmooth == 1",
                                numericInput("derLowessAlpha", 
                                             "Lowess smoothing parameter:",
                                             min= 0, max= 1, 
                                             value= .3, step= .1)),
               checkboxInput("der2order", "Plot against other input variable", 
                             FALSE),
               conditionalPanel("input.der2order == 1",
                                selectInput("dervarchoice2order", 
                                            "Other variable", 
                                            choices= "(First train)")
               ),
               plotOutput("derplot")
      )
    )
  )

))