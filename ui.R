# Define UI for dataset viewer application
shinyUI(pageWithSidebar(
  # Application title
  headerPanel("MLP"),
  
  sidebarPanel(p(HTML("Free Online Multilayer Perceptron <br /> 
                      (alpha version)"))),

  mainPanel(
    tabsetPanel(

      tabPanel("Preview Data",
               h2("Data importation"),
               p(HTML("To run the application, import your data set using the 
                import button on the left panel. Your data must be supplied 
                in the form of a text/csv file. If the importation is done 
                properly, a preview of the data is displayed below as a 
                table. When this is done, you can proceed to the next step: 
                Train Net")),
               fileInput('file1', 'Choose CSV/TXT File'),
               checkboxInput('header', ' Header?', TRUE),
               checkboxInput('rownames', ' Row names?', FALSE),
               selectInput('sep', 'Separator:', 
                           c(Comma=',', Semicolon=';', Tab='\t', Space=' '),
                           'Comma'),
               selectInput('quote', 'Quote:',  
                           c(None='', 'Double Quote'='"', 'Single Quote'="'"), 
                           'Double Quote'),
               selectInput('dec', 'Decimal mark', c(Period='.', Comma=','), 
                           'Period'),

               numericInput('nrow.preview', 'Number of rows in the preview:', 20),
               numericInput('ncol.preview', 'Number of columns in the preview:', 10),
               helpText("Note: while the preview will show only the specified number 
                         of observations, the map will be based on the full dataset."),
               tableOutput("view")
      ),

      tabPanel("Train Net",
               h3("Second step: train the net"),
               selectInput("algo", "Type of net :", 
                           choices= c("mlp", "elm", "nnet")),
               actionButton("trainbutton","Train"),
               verbatimTextOutput("summary"),
               br(),
               h4("Options"),
               numericInput("ncommittee", "Number of neural networks in the
                            committee:", value= 1, min= 1),
               selectInput("varchoicein", "Choose input variables:",
                           "(first import file)",
                           multiple= TRUE),
               selectInput("varchoiceout", "Choose output variables:",
                           "(first import file)",
                           multiple= TRUE),
               conditionalPanel("input.algo != 'mlp'",
                                numericInput("nhid", 
                                             "Number of neurons in (single) hidden layer:",
                                             5, min= 1)),
               conditionalPanel("input.algo == 'mlp'", 
                 selectInput("nhidlay", "Number of hidden layers:", 
                             choices= 1:4),
                 conditionalPanel("input.nhidlay >= 1", 
                                  numericInput("nhid1", 
                                               "Number of neurons in hidden
                                             layer 1:", 
                                               5, min= 1)),
                 conditionalPanel("input.nhidlay >= 2", 
                                  numericInput("nhid2", 
                                               "Number of neurons in hidden
                                             layer 2:", 
                                               5, min= 1)),
                 conditionalPanel("input.nhidlay >= 3", 
                                  numericInput("nhid3", 
                                               "Number of neurons in hidden
                                             layer 3:", 
                                               5, min= 1)),
                 conditionalPanel("input.nhidlay >= 4", 
                                  numericInput("nhid4", 
                                               "Number of neurons in hidden
                                             layer 4:", 
                                               5, min= 1))
               ),

               h4("Advanced options"),
               numericInput("maxit", "Max. iterations:", 100),
               uiOutput("ntrain"),
               numericInput("regul", "Regularization parameter", 0, min= 0,
                            step= .1),
               selectInput("activhid", "Activation hidden:", 
                           choices= c("htan", "logistic")),
               selectInput("activout", "Activation output:", 
                           choices= c("identity", "softmax")),
               numericInput("randseed", 
                            "Set a random seed for reproducible results:",
                            sample(1:1e5, size= 1))
      ),
      
      tabPanel("Diagnostics",
               selectInput("diagplottype", "Type of plot:",
                           choices= list("Training error"= "trainerr",
                                         "Actual vs Fitted"= "actfit",
                                         "Error distribution"= "errdistr",
                                         "Residual vs Fitted"= "residfit",
                                         "Residual vs Predictor"= "residinput")),
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
                           choices= c("test", "training", "whole")),
               selectInput("predvarchoicein", "Input variable", 
                           choices= "(First train)"),
               selectInput("predvarchoiceout", "Output variable", 
                           choices= "(First train)"),
               checkboxInput("predshowobs", "Show actual observations", FALSE),
               checkboxInput("predspline", "Show smoothing spline", TRUE),
               plotOutput("predplot")
               ),
      
      tabPanel("Partial derivatives",
               actionButton("derbutton", "Predict"),
               downloadButton("derdownload", "Download partial derivatives"),
               verbatimTextOutput("dertext"),
               selectInput("dersample", "Observations sample:",
                           choices= c("test", "training", "whole")),
               selectInput("dervarchoicein", "Input variable", 
                           choices= "(First train)"),
               selectInput("dervarchoiceout", "Output variable", 
                           choices= "(First train)"),
               checkboxInput("derspline", "Show smoothing spline", TRUE),
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