library(SwarmNet)
library(elmNN)
library(nnet)

# Max file input size :
options(shiny.maxRequestSize= 30*1024^2)

# Option choices
choices.activout <- list("mlp"= c("identity", "softmax"),
                         "nnet"= c("identity", "softmax"),
                         "elm"= "identity")
choices.activhid <- list("mlp"= c("logistic", "htan"),
                         "nnet"= "logistic",
                         "elm"= "logistic")
choices.diagplottype <- {
  list("mlp"= list("Training error"= "trainerr",
                   "Actual vs Fitted"= "actfit",
                   "Error distribution"= "errdistr",
                   "Residual vs Fitted"= "residfit",
                   "Residual vs Predictor"= "residinput"),
       "nnet"= list("Actual vs Fitted"= "actfit",
                    "Error distribution"= "errdistr",
                    "Residual vs Fitted"= "residfit",
                    "Residual vs Predictor"= "residinput"),
       "elm"= list("Actual vs Fitted"= "actfit",
                   "Error distribution"= "errdistr",
                   "Residual vs Fitted"= "residfit",
                   "Residual vs Predictor"= "residinput"))
}

# training function
trainTheNet <- function(tmp.matrix, noms.in, noms.out, hidden, niter,
                        activ.hid, activ.out, rand.seed, train, test, regul,
                        ncommittee, algo) {
  #  set.seed(rand.seed)
  res= NULL
  for (i_comm in 1:ncommittee) {
    if (algo == "mlp") {
      res= c(res, list(FBackpropNet(tmp.matrix, hidden= hidden, 
                                    noms_in= noms.in, 
                                    noms_out= noms.out,
                                    activ= activ.hid, activ_out= activ.out, 
                                    epochs= niter,
                                    train= train, test= test, regul= regul)))
    }
    if (algo == "elm") {
      res= c(res, list(elmtrain(x= tmp.matrix[train, noms.in],
                                y= tmp.matrix[train, noms.out],
                                nhid= hidden,
                                actfun= "sig")))
    }
    if (algo == "nnet") {
      res= c(res, list(nnet(x= tmp.matrix[train, noms.in],
                            y= tmp.matrix[train, noms.out],
                            size= hidden, maxit= niter, decay= regul,
                            linout= (activ.out == "identity"),
                            softmax= (activ.out == "softmax"),
                            trace= FALSE)))
    }
  }
  
  res  
}

## prediction function
predictTheNet <- function(net, newdata, algo, noms.in) {
  if (algo == "mlp")
    return(FPredictNet(net$net, newdata))
  if (algo %in% c("elm", "nnet"))
    return(predict(net, as.matrix(newdata[,noms.in])))
}

## partial derivatives function for nnet and elm
# TODO : speed up
partialDer <- function(input, matA, matB, index.in, index.out, 
                       activHid, activOut, standard.in, standard.out) {
  if (activHid == "logistic") {
    actHidFun <- function(x) 1/(1+exp(-x))
    actHidDer <- function(x) {
      tmp <- actHidFun(x)
      tmp * (1-tmp)
    }
  } else stop("wrong activHid")
  if (activOut == "identity") {
    actOutFun <- identity
    actOutDer <- function(x) matrix(1, nrow= nrow(x), ncol= ncol(x))
  } else if (activOut != "softmax") stop("wrong activout")
  
  inputH <- as.matrix(cbind(1, input)) %*% matA
  matH <- actHidFun(inputH)
  derH <- actHidDer(inputH)
  inputO <- as.matrix(cbind(1, matH)) %*% matB

  if (activOut != "softmax") {
    derO <- actOutDer(inputO)
    
    res <- as.matrix(derO[, index.out] * 
                       rowSums(derH %*% diag(matA[index.in + 1, ] * 
                                               matB[-1, index.out]) ))
    res <-   (standard.out/standard.in) * res
  } else {
    out <- exp(inputO)
    out <- out / rowSums(out)
    res <- out[, index.out] * rowSums((derH %*% diag(matA[index.in + 1, ])) *
                                        t(-t(out %*% t(matB[-1, ])) + 
                                            matB[-1, index.out]))
    res <- res/standard.in
  }
  res
}

# table of horizontal radiobuttons
tableHorizRadio <- function(names, labels, choices) {
  tmp.text <- "<TABLE cellpadding= 10> "
  for (i_name in 1:length(names)){
    tmp.line= paste("<TR> <TD> <b>", names[i_name], "</b> </TD>")
    for (i_choice in 1:length(choices))
      tmp.line <- paste(tmp.line, 
                        paste('<TD><Input type = radio Name =',
                              labels[i_name], ' Value =', 
                              paste('"', choices[i_choice], '"', sep=""),
                              if(i_choice == 1) 'checked', 
                              '</TD>', choices[i_choice]))
    tmp.text <- paste(tmp.text, tmp.line, "</TR>")
  }
  tmp.text <- paste(tmp.text, '</TD></TABLE>')
  HTML(tmp.text)
}

# Server
shinyServer(function(input, output, session) {
  # server environment variables
  server.env <- environment() # used to allocate in functions
  current.all.data <- NULL # with na
  crt.train.clicks <- 0 # number of times train button was clicked
  crt.der.clicks <- 0 # number of times der button was clicked
  crt.fits <- list() # list of trained nets
  crt.n.fits <- 0 # number of trained nets
  active.fit <- NULL # current net, taken from crt.fits
  crt.var.types <- NULL # current chosen types of variables
  
  ##############################################################################
  ## Left panel
  
  # Store current fit in active.fit
  observe({
    input$fit # (fit selector is updated at the end of each training)
    if(crt.n.fits == 0) return(NULL)
    server.env$active.fit <- crt.fits[[input$fit]]
  })
  
  # Summary of current fit
  output$summary <- renderPrint({ 
    input$fit
    if(crt.n.fits == 0) return(NULL)
    
    cat(" Training sample MSE :", {
      mean((active.fit$pred[active.fit$train,] -
              as.matrix(active.fit$data[active.fit$train, active.fit$namesout])
            )**2)
      },"\n",
        "Test sample MSE     :", {
          mean((active.fit$pred[active.fit$test,] -
                  as.matrix(active.fit$data[active.fit$test, 
                                            active.fit$namesout]))**2)
        },
        "\n\n Output variable(s):", paste(active.fit$namesout, collapse= ", "), 
        "\n",
        "\n Input variable(s):", paste(active.fit$matnamesin, collapse= ", "), 
        "\n\n",
        "Training algorithm        :", active.fit$algo, '\n',
        "Number of networks        :", active.fit$ncommittee, '\n',
        "Hidden neurons per net    :", active.fit$hidden, '\n', 
        if (active.fit$algo != "elm") paste(
          "Max. number of iterations :", active.fit$maxit, '\n',
          "Weight decay parameter    :", active.fit$regul, '\n' 
        ),
        "Hidden neurons activation :", active.fit$activhid, "\n",
        "Output neurons activation :", active.fit$activout, "\n",
        "Seed                      :", active.fit$randseed, '\n\n',
        if (nrow(current.all.data) != nrow(active.fit$data)) {
          paste("\n", "Warning:", 
                nrow(current.all.data) - nrow(active.fit$data), 
                "observations removed because of NA values.\n")
        } else {""}, "\n",
        if (active.fit$ncommittee > 1) 
          "Individual committee members errors\n", 
        if (active.fit$ncommittee > 1) 
          as.matrix(sapply(active.fit$net, 
                           function(x) switch(active.fit$algo,
                                              "mlp"= x$MSE_final,
                                              "elm"= mean(x$residuals**2),
                                              "nnet"= mean(x$residuals**2))))          
    )
  })
  
  
  ##############################################################################
  ## File input tab
  
  dInput <- reactive({
    in.file <- input$file1
    
    if (is.null(in.file))
      return(NULL)
    
    if (input$rownames) {
      the.table <- read.table(in.file$datapath, header=input$header, 
                              sep=input$sep, quote=input$quote, 
                              row.names=1, dec=input$dec)
    } else the.table <- read.table(in.file$datapath, header=input$header, 
                                   sep=input$sep, quote=input$quote, 
                                   dec=input$dec)
    
    # clear trained networks
    server.env$crt.fits <- list()
    server.env$crt.n.fits <- 0
    server.env$active.fit <- NULL
    updateSelectInput(session, "fit", choices= "(No trained networks)")
    
    
    server.env$current.all.data <- the.table
    the.table
  })
  
  # data preview table
  output$view <- renderTable({
    d.input <- dInput()
    if (is.null(d.input)) 
      return(NULL)
    if (ncol(d.input)>input$ncol.preview) 
      d.input <- d.input[,1:input$ncol.preview]
    head(d.input, n=input$nrow.preview) 
  })
  
  ##############################################################################
  ## Training tab
  
  output$trainMessage <- renderPrint({
    cat("Hit the Train button to train the neural network.")
  })
  
  # File upload updates variable choice list and size of training sample
  observe({
    if (is.null(input$file1)) return(NULL)

    output$varchoice <- renderUI({
      tableHorizRadio(names= colnames(current.all.data), 
                      labels= paste("var", 1:ncol(current.all.data), sep=""),
                      choices= c("Not used",
                                 "Numeric Input",
                                 "Categorical Input",
                                 "Numeric Output",
                                 "Categorical Output"))
#                       choices= c("Not used",
#                                  "Input",
#                                  "Output"))
    })
    
    output$ntrain <- renderUI({
      numericInput(inputId= "ntrain", 
                   label= "Number of training samples:", 
                   min= 1, step= 1,
                   max= nrow(current.all.data),
                   value= round(.8*nrow(current.all.data), 0))
    })
  })
  
  # Adapt choice of activations to choice of model (same selected if possible)
  observe({
    updateSelectInput(session, "activhid", 
                      choices= choices.activhid[[input$algo]],
                      selected= ifelse(input$activhid %in% 
                                         choices.activhid[[input$algo]],
                                       input$activhid, 1))
    updateSelectInput(session, "activout", 
                      choices= choices.activout[[input$algo]],
                      selected= ifelse(input$activout %in% 
                                         choices.activout[[input$algo]],
                                       input$activout, 1))
  })
  
  # Train the net when the button is hit
  ## TODO : fix 2-level factors
  theTrain<- observe({ if(input$trainbutton > crt.train.clicks) {
    dInput()
    if(is.null(current.all.data)) {
      output$trainMessage <- renderPrint({
        cat("Error : first import a dataset.")
      })
      server.env$crt.train.clicks <- input$trainbutton
      return(NULL)
    }
    
    tmp.selvars= c(crt.var.types[["Numeric Input"]], 
                   crt.var.types[["Numeric Output"]],
                   crt.var.types[["Categorical Input"]],
                   crt.var.types[["Categorical Output"]])
#     tmp.selvars= c(crt.var.types[["Input"]], 
#                    crt.var.types[["Output"]])

    tmp.varchoicein <- c(crt.var.types[["Numeric Input"]], 
                         crt.var.types[["Categorical Input"]])
    tmp.varchoiceout <- c(crt.var.types[["Numeric Output"]], 
                          crt.var.types[["Categorical Output"]])
#     tmp.varchoicein <- c(crt.var.types[["Input"]])
#     tmp.varchoiceout <- c(crt.var.types[["Output"]])
    if (is.null(tmp.varchoicein) | is.null(tmp.varchoiceout)) {
      output$trainMessage <- renderPrint(cat("Error : select at least one",
                                             "input and one output variable."))
      server.env$crt.train.clicks <- input$trainbutton
      return(NULL)
    }
    
    # remove from working data the obs where input or output are NA
    tmp.data <- server.env$current.all.data[
      rowSums(is.na(current.all.data[, tmp.selvars])) == 0, ]
    
    # check for constants
    tmp.constants <- sapply(tmp.data[, tmp.selvars], 
                            function(x) length(unique(x)) < 2)
    if (any(tmp.constants)) {
      output$trainMessage <- renderPrint({
        cat("Error : variable(s)", 
            paste(tmp.selvars[tmp.constants], collapse= ","),
            "are constant.")        
      })
      return(NULL)
    }
    
    # make variables conform to specified types
    for (i_var in c(crt.var.types[["Categorical Input"]],
                    crt.var.types[["Categorical Output"]]))
      tmp.data[, i_var] <- as.factor(tmp.data[, i_var])
    for (i_var in c(crt.var.types[["Numeric Input"]],
                    crt.var.types[["Numeric Output"]]))
      tmp.data[, i_var] <- as.numeric(tmp.data[, i_var])
    
    set.seed(input$randseed)
    tmp.train <- sample(1:nrow(tmp.data), size= input$ntrain)
    tmp.test <- (1:nrow(tmp.data))[-tmp.train]

    tmp.matrix <- model.matrix(as.formula(object= paste("~ 0+", 
                                                        paste(tmp.varchoicein, 
                                                              collapse= "+") )),
                               data= tmp.data)
    tmp.rownames <- rownames(tmp.matrix)
    tmp.matnamesin <- colnames(tmp.matrix)
    tmp.datnamesin <- tmp.varchoicein
    tmp.namesout <- tmp.varchoiceout
    tmp.matrix <- as.matrix(cbind(tmp.data[rownames(tmp.matrix),
                                               tmp.varchoiceout], 
                                  tmp.matrix))
    colnames(tmp.matrix) <- c(tmp.namesout, tmp.matnamesin)
    rownames(tmp.matrix) <- tmp.rownames

    # normalize
    tmp.varin.range <- sapply(as.data.frame(tmp.matrix[tmp.train, 
                                                       tmp.matnamesin]), 
                              FUN= range)
    colnames(tmp.varin.range) <- tmp.matnamesin
    for (i_var in tmp.matnamesin)
      tmp.matrix[,i_var] <- {
        2 * ( (tmp.matrix[, i_var] - tmp.varin.range[1, i_var] ) /
                (tmp.varin.range[2, i_var] - tmp.varin.range[1, i_var]) ) - 1
      }
    
    tmp.varout.mean <- 
      sapply(as.data.frame(tmp.matrix[tmp.train, tmp.namesout]), FUN= mean)
    names(tmp.varout.mean) <- tmp.namesout

    tmp.varout.sd <- 
      sapply(as.data.frame(tmp.matrix[tmp.train, tmp.namesout]), FUN= sd)
    names(tmp.varout.sd) <- tmp.namesout
    if (input$activout != "softmax") for (i_var in tmp.namesout)
      tmp.matrix[, i_var] <- {
        (tmp.matrix[, i_var] - tmp.varout.mean[i_var]) / tmp.varout.sd[i_var]
      }
    
    if (input$algo == "mlp") {
      tmp.hidden <- c(input$nhid1, input$nhid2, 
                      input$nhid3, 
                      input$nhid4)[1:input$nhidlay]
    } else tmp.hidden <- input$nhid1
    
    tmp.net <- trainTheNet(tmp.matrix, noms.in= tmp.matnamesin, 
                           noms.out= tmp.varchoiceout, 
                           niter= input$maxit, 
                           activ.hid= input$activhid, 
                           activ.out= input$activout,
                           rand.seed= input$randseed, train= tmp.train, 
                           test= tmp.test, regul= input$regul,
                           ncommit= input$ncommittee, algo= input$algo,
                           hidden= tmp.hidden)
    
    # predict
    tmp.pred <- matrix(0, ncol= length(tmp.varchoiceout),
                                      nrow= nrow(tmp.matrix))
    for (i_commi in 1:input$ncommittee)
      tmp.pred <- tmp.pred + predictTheNet(
        net= tmp.net[[i_commi]], newdata= tmp.matrix, algo= input$algo, 
        noms.in= tmp.matnamesin) / input$ncommittee
    
    # reverse normalization of prediction
    if (input$activout != "softmax") for (i_var in 1:ncol(tmp.pred))
      tmp.pred[, i_var] <- {
        (tmp.pred[, i_var] * tmp.varout.sd[i_var]) + tmp.varout.mean[i_var]
      }
    
    # Save net into fits list
    server.env$crt.n.fits <- crt.n.fits + 1
    server.env$crt.fits[[crt.n.fits]] <- {
      list(net= tmp.net,
           data= tmp.data, # not necessary?
           train= tmp.train,
           test= tmp.test,
           matrix= tmp.matrix,
           pred= tmp.pred,
           matnamesin= tmp.matnamesin,
           datnamesin= tmp.varchoicein,
           namesout= tmp.varchoiceout,
           varin.range= tmp.varin.range,
           varout.mean= tmp.varout.mean,
           varout.sd= tmp.varout.sd,
           algo= input$algo,
           ncommittee= input$ncommittee,
           activhid= input$activhid,
           activout= input$activout,
           randseed= input$randseed,
           hidden= tmp.hidden,
           regul= input$regul,
           maxit= input$maxit)
    }
    names(server.env$crt.fits)[crt.n.fits] <- paste(crt.n.fits, "-", input$algo)
    
    output$trainMessage <- renderPrint({
      cat(" Training successful. (Name:", names(crt.fits)[crt.n.fits], ")\n",
          "You may train another neural network to compare results.")
    })
      
    # Update models list on the left
    updateSelectInput(session, "fit", choices= rev(names(crt.fits)))
    
    # Update train button counter
    server.env$crt.train.clicks <- input$trainbutton
  }})
  
  # Variables message
  output$varMessage <- renderPrint({
    input$fit # (fit selector is updated at the end of each training)
    dInput()
    if (is.null(input$file1))
      return(cat("First import a dataset."))
    input$trainbutton
    
    var.labels <- paste("var", 1:ncol(current.all.data), sep="")
    tmp.vars <- list("Not used"= NULL, "Numeric Output"= NULL,
                     "Numeric Input"= NULL, "Categorical Output"= NULL,
                     "Categorical Input"= NULL)
#     tmp.vars <- list("Not used"= NULL, "Output"= NULL, "Input"= NULL)
    for (i_var in 1:ncol(current.all.data))
      tmp.vars[[ input[[ var.labels[i_var] ]] ]] <- {
        c(tmp.vars[[input[[var.labels[i_var]]]]],
          colnames(current.all.data)[i_var])
      }
    
    if (length(tmp.vars[c("Numeric Output", "Categorical Output")]) == 0 &
          length(tmp.vars[c("Numeric Input", "Categorical Input")]) == 0 )
#     if (length(tmp.vars[c("Output")]) == 0 & length(tmp.vars[c("Input")]) == 0 )
      return(cat("Select at least one input and one output variable"))
    
    for (i_type in c("Numeric Input", "Categorical Input",
                     "Numeric Output", "Categorical Output"))
#     for (i_type in c("Output", "Input"))
#       if (!is.null(tmp.vars[[i_type]]))
        cat(i_type, ":", paste(tmp.vars[[i_type]], collapse= ", "), "\n")
    
    server.env$crt.var.types <- tmp.vars
  })
  
  
  ##############################################################################
  ## Diagnostics
    
  # update available variables and plots in diagno, same selected if possible
  updateDiagOptions= reactive({
    input$fit
    
    updateSelectInput(session, "diagvarchoiceout",
                      choices= active.fit$namesout,
                      selected= if (input$diagvarchoiceout %in% 
                                      active.fit$namesout) {
                        input$diagvarchoiceout
                      } else 1)
    updateSelectInput(session, "diagvarchoicein",
                      choices= active.fit$datnamesin,
                      selected= if (input$diagvarchoicein %in% 
                                      active.fit$datnamesin) {
                        input$diagvarchoicein
                      } else 1)
    
    updateSelectInput(session, "diagplottype", 
                      choices= choices.diagplottype[[active.fit$algo]],
                      selected=
                        names(choices.diagplottype[[active.fit$algo]])[
                          input$diagplottype == 
                            unlist(choices.diagplottype[[active.fit$algo]])] )
  })
  
  # Diagnostics plot
  output$diagplot <- renderPlot({
    input$fit
    if (crt.n.fits == 0) return(NULL)

    updateDiagOptions()
    
    tmp.sample <- switch(input$diagsample,
                         "training"= active.fit$train, 
                         "test"= active.fit$test,
                         "whole"= c(active.fit$train, active.fit$test))
    switch(input$diagplottype, 
           "trainerr"= {
             if (active.fit$algo != "mlp") return(NULL)
             plot(active.fit$net[[1]]$evol_err, 
                  ylim= range(c(active.fit$net[[1]]$evol_err, 
                                active.fit$net[[1]]$evol_test)),
                  xlab= "iteration", ylab= "training error")
             lines(active.fit$net[[1]]$evol_test, t="p", col= 2)
             legend("topright", col= 1:2, pch= c(1,1), 
                    c("Training sample error", "Test sample error"))
           },
           "actfit"= {
             plot(active.fit$pred[tmp.sample, 
                               if(ncol(active.fit$pred) == 1) {1} else 
                                 which(active.fit$namesout == 
                                         input$diagvarchoiceout)],
                  active.fit$data[tmp.sample, input$diagvarchoiceout],
                  xlab= paste("predicted", input$diagvarchoiceout),
                  ylab= paste("observed", input$diagvarchoiceout))
             abline(0,1,col=2,lwd=2,lty=2)
             legend("topleft", col= 2, lwd= 2, lty= 2, "y = x")
           },
           "residfit"= {
             tmp.resid <- active.fit$data[tmp.sample, input$diagvarchoiceout] -
               active.fit$pred[tmp.sample, 
                               if(ncol(active.fit$pred) == 1) {1} else 
                                 which(active.fit$namesout == 
                                         input$diagvarchoiceout)]
                               
             plot(active.fit$pred[tmp.sample, 
                               if(ncol(active.fit$pred) == 1) {1} else 
                                 (1:ncol(active.fit$pred))[
                                   active.fit$namesout == 
                                     input$diagvarchoiceout]],
                  tmp.resid,
                  xlab= paste("predicted", input$diagvarchoiceout),
                  ylab= paste("prediction error for", input$diagvarchoiceout),
                  main= paste("mean error:",
                              mean(tmp.resid), "\nmean absolute deviation:",
                              mean(abs(tmp.resid)), 
                              "\nroot mean squared error:",
                              sqrt(mean(tmp.resid**2))))
             abline(h=0,col=2,lwd=2,lty=2)
             legend("bottom", col=2,lwd=2,lty=2, "error = 0")
           },
           "residinput"= {
             tmp.resid <- active.fit$data[tmp.sample, input$diagvarchoiceout] -
               active.fit$pred[tmp.sample, 
                            if(ncol(active.fit$pred) == 1) {1} else 
                              which(active.fit$namesout == 
                                      input$diagvarchoiceout)]
             plot(active.fit$data[tmp.sample, input$diagvarchoicein], 
                  tmp.resid,
                  xlab= paste("observed", input$diagvarchoicein),
                  ylab= paste("prediction error for", input$diagvarchoiceout),
                  main= paste("mean error:",
                              mean(tmp.resid), "\nmean absolute deviation:",
                              mean(abs(tmp.resid)), 
                              "\nroot mean squared error:",
                              sqrt(mean(tmp.resid**2)))
             )
             abline(h=0,col=2,lwd=2,lty=2)
             legend("bottom", lty= 2, lwd= 2, col= 2, "error = 0")
           },
           "errdistr"= {
             tmp.resid <- active.fit$data[tmp.sample, input$diagvarchoiceout] -
               active.fit$pred[tmp.sample, 
                            if(ncol(active.fit$pred) == 1) {1} else 
                              which(active.fit$namesout == 
                                      input$predvarchoiceout)]
             plot(density(tmp.resid, bw= "SJ"),
                  xlab= paste("prediction error for", input$diagvarchoiceout),
                  ylab= paste("density"), main= "prediction error density")
             rug(tmp.resid)
             abline(v=0, col= 2, lty= 2)
             legend("topleft", col= 2, lty= 2, "error = 0")
           }
           
    )
    
  })
  
  ##############################################################################
  ## Prediction
  
  updateVarPred <- reactive({
    input$fit
    updateSelectInput(session, "predvarchoicein",
                      choices= active.fit$datnamesin,
                      selected= if (input$predvarchoicein %in% 
                                      active.fit$datnamesin) {
                        input$predvarchoicein
                      } else 1)
    updateSelectInput(session, "predvarchoiceout",
                      choices= active.fit$namesout,
                      selected= if (input$predvarchoiceout %in% 
                                      active.fit$namesout) {
                        input$predvarchoiceout
                      } else 1)
  })
  
  # plot predictions
  output$predplot <- renderPlot({
    input$fit
    if (crt.n.fits == 0) return(NULL)
    updateVarPred()
    
    tmp.sample <- switch(input$predsample,
                         "training"= active.fit$train, 
                         "test"= active.fit$test,
                         "whole"= c(active.fit$train, active.fit$test))
    tmp.tab <- data.frame(active.fit$data[tmp.sample, input$predvarchoicein],
                          active.fit$pred[tmp.sample, {
                            if(ncol(active.fit$pred) == 1) {1} else 
                              which(active.fit$namesout == 
                                      input$predvarchoiceout)
                          }])
    plot(tmp.tab, 
         xlab= input$predvarchoicein, 
         ylab= input$predvarchoiceout,
         ylim= range(active.fit$data[tmp.sample, input$predvarchoiceout]))

    if (input$predsmooth) {
      tmp.lowess <- lowess(x= tmp.tab[, 1], y= tmp.tab[, 2], 
                           f= ifelse(input$predLowessAlpha > 0, 
                                     input$predLowessAlpha, 1e-4))
      lines(cbind(tmp.lowess$x, tmp.lowess$y)[order(tmp.lowess$x),], 
            col= 2, lwd= 2)
    }
    if(input$predshowobs == TRUE) {
      lines(active.fit$data[tmp.sample, c(input$predvarchoicein, 
                                          input$predvarchoiceout)],
            col= 3, pch= 2, t= "p")
      if (input$predsmooth) {
        tmp.lowess <- lowess(x= active.fit$data[tmp.sample, 
                                                input$predvarchoicein],
                             y= active.fit$data[tmp.sample, 
                                                input$predvarchoiceout],
                             f= ifelse(input$predLowessAlpha > 0, 
                                          input$predLowessAlpha, 1e-4))
        lines(cbind(tmp.lowess$x, tmp.lowess$y)[order(tmp.lowess$x),], 
              col= 4, lwd= 2)
      }
    }
  })
  
  # Download predictions
  output$preddownload <- {
    downloadHandler(filename= function() {
      paste("mlp_pred_", format(Sys.time(), format= "-%Y-%m-%d_%H:%M"),
            ".csv", sep="")
    }, 
                    content= function(file) {
                      write.csv(active.fit$pred, file= file, 
                                row.names= rownames(current.all.data)[
                                  rownames(current.all.data) 
                                  %in% rownames(active.fit$data)],
                                col.names= active.fit$namesout)
                    })
  }
  
  
  ##############################################################################
  ## Partial derivatives
  # TODO : categorical inputs
  
  updateVarDer <- reactive({
    input$fit
    updateSelectInput(session, "dervarchoicein",
                      choices= active.fit$matnamesin[active.fit$matnamesin %in% 
                                                       active.fit$datnamesin],
                      selected= if (input$dervarchoicein %in% 
                                      active.fit$matnamesin[active.fit$matnamesin %in% 
                                                              active.fit$datnamesin]) {
                        input$dervarchoicein
                      } else 1)
    updateSelectInput(session, "dervarchoiceout",
                      choices= active.fit$namesout,
                      selected= if (input$dervarchoiceout %in% 
                                      active.fit$namesout) {
                        input$dervarchoiceout
                      } else 1)
    updateSelectInput(session, "dervarchoice2order",
                      choices= active.fit$datnamesin,
                      selected= if (input$dervarchoice2order %in% 
                                      active.fit$datnamesin) {
                        input$dervarchoice2order
                      } else 1)
  })
  
  # launch derivatives function
  computeDer <- function(algo, dervarchoicein, dervarchoiceout, ncommittee,
                         activhid, activout) {
    if (algo == "mlp") { #  all input variables at once with FNetPartial
      tmp.der <- 0
      for (i_commi2 in 1:ncommittee) {
        tmp.der <- tmp.der + 
          FNetPartial(le_net= active.fit$net[[i_commi2]], 
                      data_in= active.fit$matrix,
                      id_var= if(ncol(active.fit$pred) == 1) {0} else 
                        (1:length(active.fit$namesout))[
                          active.fit$namesout ==
                            dervarchoiceout] - 1) / 
          ncommittee
        
      }
      for (i_var in active.fit$matnamesin)
        tmp.der[, i_var] <- tmp.der[, i_var] * 2 * 
        (active.fit$varout.sd[which(active.fit$namesout == dervarchoiceout)] / 
           (active.fit$varin.range[2, i_var] - 
              active.fit$varin.range[1, i_var]))
    } else if (algo == "nnet") {
      tmp.der <- 0
      matASubset= 1:(active.fit$net[[1]]$n[2] * (1 + active.fit$net[[1]]$n[1]))
      matBSubset= {
        (1 + active.fit$net[[1]]$n[2] * 
           (1 + active.fit$net[[1]]$n[1])):length(active.fit$net[[1]]$wts) 
      }
      for (i_commi2 in 1:ncommittee) {
        tmp.der <- tmp.der + 
          partialDer(input= active.fit$matrix[, active.fit$matnamesin], 
                     matA= matrix(active.fit$net[[i_commi2]]$wts[matASubset],
                                  ncol= active.fit$net[[i_commi2]]$n[2]),
                     matB= matrix(active.fit$net[[i_commi2]]$wts[matBSubset],
                                  ncol= active.fit$net[[i_commi2]]$n[3]),
                     index.in= which(active.fit$matnamesin == 
                                       dervarchoicein),
                     index.out= which(active.fit$namesout ==
                                        dervarchoiceout),
                     activHid= active.fit$activhid,
                     activOut= active.fit$activout,
                     standard.in= {
                       (active.fit$varin.range[2, dervarchoicein] - 
                          active.fit$varin.range[1, dervarchoicein]) / 2 
                     },
                     standard.out= {
                       active.fit$varout.sd[which(active.fit$namesout == 
                                                  dervarchoiceout)]
                     }) / ncommittee 
      }
    } else if (algo == "elm") {
      tmp.der <- 0
      for (i_commi in 1:ncommittee) {
        tmp.der <- tmp.der + 
          partialDer(input= active.fit$matrix[, active.fit$matnamesin], 
                     matA= {
                       as.matrix(rbind(active.fit$net[[i_commi]]$biashid, 
                                       t(active.fit$net[[i_commi]]$inpweight))) 
                     },
                     matB= rbind(0, active.fit$net[[i_commi]]$outweight),
                     index.in= which(active.fit$matnamesin == dervarchoicein),
                     index.out= which(active.fit$namesout == dervarchoiceout),
                     activHid= active.fit$activhid,
                     activOut= active.fit$activout,
                     standard.in= {
                       (active.fit$varin.range[2, dervarchoicein] - 
                          active.fit$varin.range[1, dervarchoicein]) / 2 
                     },
                     standard.out= {
                       active.fit$varout.sd[which(active.fit$namesout == 
                                                  dervarchoiceout)]
                     }) / ncommittee 
      }
    }
    as.matrix(tmp.der)
  }
  
  # compute derivatives when button is hit
  observe({
    input$fit
    if (crt.n.fits == 0) return(NULL)
    input$derbutton
    if (!is.null(active.fit$der)) {
      server.env$crt.der.clicks <- input$derbutton
      return(NULL)
    }
    if (input$derbutton > crt.der.clicks) {
      for (i_varout in active.fit$namesout) {
        tmp.der <- NULL
        if (active.fit$algo == "mlp") {
          tmp.der <- computeDer(active.fit$algo, 
                                NULL, 
                                i_varout, 
                                active.fit$ncommittee, 
                                active.fit$activhid, 
                                active.fit$activout)
        } else {
          for (i_varin in active.fit$matnamesin[active.fit$matnamesin %in% 
                                                  active.fit$datnamesin]){
            tmp.der <- cbind(tmp.der, computeDer(active.fit$algo, 
                                                 i_varin, 
                                                 i_varout, 
                                                 active.fit$ncommittee, 
                                                 active.fit$activhid, 
                                                 active.fit$activout))
          }
          tmp.der <- as.matrix(tmp.der)
          colnames(tmp.der) <- {
            active.fit$matnamesin[active.fit$matnamesin %in% 
                                    active.fit$datnamesin] 
          }
        }
        server.env$active.fit$der[[i_varout]] <- tmp.der
        server.env$crt.fits[[input$fit]]$der[[i_varout]] <- tmp.der
      }
      server.env$crt.der.clicks <- input$derbutton
    }
  })
  
  # message about state of derivatives
  output$dertext <- renderPrint({
    input$fit
    if (crt.n.fits == 0) return(cat("First train a net."))
    updateVarDer()
    input$derbutton
    if (is.null(active.fit$der)) 
      return(cat("Click compute button to predict derivatives."))
    
    cat("Partial derivatives of fit [", input$fit, "] successfully computed.")
    
  })
  
  # plot derivatives
  ## TODO : fix strange bug when switching between models (different plots when going up or down...)
  ## also happens for prediction plots. Plots not updated correctly, fixed when eg lowess is turned off/on
  output$derplot <- renderPlot({
    input$fit
    if (crt.n.fits == 0) return(NULL)
    input$derbutton
    if (is.null(active.fit$der)) return(NULL)
    
    tmp.sample <- switch(input$dersample,
                         "training"= active.fit$train, 
                         "test"= active.fit$test,
                         "whole"= c(active.fit$train, active.fit$test))
    
    tmp.varin <- ifelse(input$der2order, 
                        input$dervarchoice2order,
                        input$dervarchoicein)
    
    plot(active.fit$data[tmp.sample, tmp.varin],
         active.fit$der[[input$dervarchoiceout]][tmp.sample, 
                                                 input$dervarchoicein],
         xlab= tmp.varin, 
         ylab= paste("partial (", input$dervarchoiceout, 
                     "/", input$dervarchoicein,")"),
         ylim= range(c(0, {
           active.fit$der[[input$dervarchoiceout]][tmp.sample, 
                                                   input$dervarchoicein]
         })))
    
    if (input$dersmooth) {
      tmp.lowess <- lowess(x= active.fit$data[tmp.sample, tmp.varin], 
                           y= active.fit$der[[input$dervarchoiceout]][
                             tmp.sample, input$dervarchoicein],
                           f= ifelse(input$derLowessAlpha > 0, 
                                     input$derLowessAlpha, 1e-4))
      lines(cbind(tmp.lowess$x, tmp.lowess$y)[order(tmp.lowess$x), ], 
            col= 2, lwd= 2)
    }
    
    abline(h=0, col= 4)
    legend("topleft", lty= 1, col= 4, "y=0")
  })
  
#   # Download derivatives
#   output$derdownload <- downloadHandler(filename= function() {
#     paste("mlp_der_", active.fit$der.varout, "_",
#           format(Sys.time(),format="-%Y-%m-%d_%H:%M"),".csv",sep="")
#   }, content= function(file) {
#     write.csv(active.fit$der, file= file, 
#               row.names= rownames(current.all.data)[rownames(current.all.data)
#                                                     %in% rownames(active.fit$data)],
#               col.names= active.fit$matnamesin)
#   })
})


