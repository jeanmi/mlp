library(SwarmNet)
library(elmNN)
library(nnet)

# Max file input size :
options(shiny.maxRequestSize= 30*1024^2)

# training function
trainTheNet <- function(tmp.matrix, noms.in, noms.out, hidden, niter,
                        activ.hid, activ.out, rand.seed, train, test, regul,
                        ncommittee, algo) {
    #  set.seed(rand.seed)
  res= NULL
  for (i_comm in 1:ncommittee) {
    if (algo == "mlp") {
      res= c(res, list(FBackpropNet(tmp.matrix, hidden= hidden, noms_in= noms.in, 
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
    return(predict(net, newdata[,noms.in]))
}

## partial derivatives function for one-hidden layer net
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
  } else stop("wrong activOut")
  
  inputH <- as.matrix(cbind(1, input)) %*% matA
  matH <- actHidFun(inputH)
  derH <- actHidDer(inputH)
    
  inputO <- as.matrix(cbind(1, matH)) %*% matB
  derO <- actOutDer(inputO)
  
  res <- as.matrix(derO * rowSums(derH %*% diag(matA[index.in + 1, ] * matB[-1,index.out]) ))
  (standard.out/standard.in) * res
}


# Server
shinyServer(function(input, output, session) {
  # server environment variables
  server.env <- environment() # used to allocate in functions
  current.all.data <- NULL # with na
  crt.train.clicks <- 0
  crt.der.clicks <- 0
  crt.fits <- list()
  crt.n.fits <- 0
  
  current.net <- NULL # this variable will contain the current net
  current.data <- NULL # without the na of input and output variables
  current.train <- NULL
  current.test <- NULL
  current.matrix <- NULL
  current.pred <- NULL
  
  current.der <- NULL
  current.der.varin <- NULL
  current.der.varout <- NULL
    
  current.matnamesin <- NULL
  current.datnamesin <- NULL
  current.namesout <- NULL
  
  crt.varin.range <- NULL
  crt.varout.mean <- NULL
  crt.varout.sd <- NULL
  
  
  # File input
  dInput <- reactive({
    in.file <- input$file1
    
    if (is.null(in.file))
      return(NULL)
    
    if (input$rownames) {
      the.table <- read.table(in.file$datapath, header=input$header, 
                              sep=input$sep, quote=input$quote, 
                              row.names=1, dec=input$dec)
    } else the.table <- read.table(in.file$datapath, header=input$header, 
                                   sep=input$sep, quote=input$quote, dec=input$dec)
    
    # update the "input variables" checkbox (if somtype is numeric or integer)
    updateVarChoiceIn()
    updateVarChoiceOut()
    updateTrainSlider()
    
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
    
  # Update in and out variable list
  updateVarChoiceIn <- function() observe({
    updateSelectInput(session, "varchoicein",
                      choices= as.list(colnames(current.all.data)))
  })
  updateVarChoiceOut <- function() observe({
    updateSelectInput(session, "varchoiceout",
                      choices= as.list(colnames(current.all.data)))
  })
  
  
  # Update trainSlider
  updateTrainSlider <- function() {
    output$ntrain <- renderUI(numericInput(inputId= "ntrain", 
                                             label= "Number of training samples:", 
                                             min= 1, step= 1,
                                             max= nrow(current.all.data),
                                             value= round(.8*nrow(current.all.data),
                                                          0)))
  }
      
  # Adapt choice of activations to choice of model
  observe({
    updateSelectInput(session, "activhid", 
                      choices= switch(input$algo,
                                      "mlp"= c("htan", "logistic"),
                                      "nnet"= "logistic",
                                      "elm"= "logistic"))    
    updateSelectInput(session, "activout", 
                      choices= switch(input$algo,
                                      "mlp"= c("identity", "softmax"),
                                      "nnet"= "identity",
                                      "elm"= "identity"))    
  })
  
  # Train the net when the button is hit
  theTrain<- observe({ if(input$trainbutton > crt.train.clicks) {
    dInput()
    if(is.null(current.all.data) |
         is.null(input$varchoicein) | is.null(input$varchoiceout)) {  
      server.env$crt.train.clicks <- input$trainbutton
      return(NULL)
    }
    
    # remove from working data the obs where input or output are NA
    tmp.data <- server.env$current.all.data[
      rowSums(is.na(current.all.data[,c(input$varchoicein, 
                                        input$varchoiceout)])) == 0, ]
    server.env$current.data <- tmp.data
    
    set.seed(input$randseed)
    tmp.train <- sample(1:nrow(tmp.data), size= input$ntrain)
    tmp.test <- (1:nrow(tmp.data))[-tmp.train]
    server.env$current.train <- tmp.train
    server.env$current.test <- tmp.test

    tmp.matrix <- model.matrix(as.formula(object= paste("~ 0+", 
                                                        paste(input$varchoicein, 
                                                              collapse= "+") )),
                               data= tmp.data)
    tmp.rownames <- rownames(tmp.matrix)
    tmp.matnamesin <- colnames(tmp.matrix)
    tmp.datnamesin <- input$varchoicein
    tmp.namesout <- input$varchoiceout
    tmp.matrix <- as.matrix(cbind(tmp.data[rownames(tmp.matrix),
                                               input$varchoiceout], 
                                  tmp.matrix))
    colnames(tmp.matrix) <- c(tmp.namesout, tmp.matnamesin)
    rownames(tmp.matrix) <- tmp.rownames
    
    server.env$current.matrix <- tmp.matrix
    server.env$current.matnamesin <- tmp.matnamesin
    server.env$current.datnamesin <- tmp.datnamesin
    server.env$current.namesout <- tmp.namesout
    
    # normalize
    tmp.varin.range <- sapply(as.data.frame(tmp.matrix[tmp.train, tmp.matnamesin]), FUN= range)
    server.env$crt.varin.range <- tmp.varin.range
    for (i_var in tmp.matnamesin)
      tmp.matrix[,i_var] <- 2 * ( (tmp.matrix[, i_var] - tmp.varin.range[1, i_var] ) /
                                    (tmp.varin.range[2, i_var] - tmp.varin.range[1, i_var]) ) - 1
    
    tmp.varout.mean <- 
      sapply(as.data.frame(tmp.matrix[tmp.train, tmp.namesout]), FUN= mean)
    names(tmp.varout.mean) <- tmp.namesout
    server.env$crt.varout.mean <- tmp.varout.mean

    tmp.varout.sd <- 
      sapply(as.data.frame(tmp.matrix[tmp.train, tmp.namesout]), FUN= sd)
    names(tmp.varout.sd) <- tmp.namesout
    server.env$crt.varout.sd <- tmp.varout.sd
    if (input$activout != "softmax") for (i_var in tmp.namesout)
      tmp.matrix[, i_var] <- (tmp.matrix[, i_var] - tmp.varout.mean[i_var]) / tmp.varout.sd[i_var]
    
    if (input$algo == "mlp") {
      tmp.hidden <- c(input$nhid1, input$nhid2, 
                      input$nhid3, 
                      input$nhid4)[1:input$nhidlay]
    } else tmp.hidden <- input$nhid
    
    tmp.net <- trainTheNet(tmp.matrix, noms.in= tmp.matnamesin, 
                           noms.out= input$varchoiceout, 
                           hidden= tmp.hidden,
                           niter= input$maxit, 
                           activ.hid= input$activhid, 
                           activ.out= input$activout,
                           rand.seed= input$randseed, train= tmp.train, 
                           test= tmp.test, regul= input$regul,
                           ncommit= input$ncommittee, algo= input$algo)
    
    
    # predict
    tmp.pred <- matrix(0, ncol= length(input$varchoiceout),
                                      nrow= nrow(tmp.matrix))
    for (i_commi in 1:input$ncommittee)
      tmp.pred <- tmp.pred + predictTheNet(
        net= tmp.net[[i_commi]], newdata= tmp.matrix, algo= input$algo, 
        noms.in= tmp.matnamesin) / input$ncommittee
    
    # reverse normalization of prediction
    if (input$activout != "softmax") for (i_var in 1:ncol(tmp.pred))
      tmp.pred[, i_var] <- (tmp.pred[, i_var] * tmp.varout.sd[i_var]) + tmp.varout.mean[i_var]
    server.env$current.pred <- tmp.pred
    
    # save net
    server.env$current.net <- tmp.net
    
    # Update variables
    updateVarDiagno()
    updateVarPred() 
    updateVarDer()
    
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
           datnamesin= input$varchoicein,
           namesout= input$varchoiceout,
           varin.range= tmp.varin.range,
           varout.mean= tmp.varout.mean,
           varout.sd= tmp.varout.sd,
           algo= input$algo,
           ncommittee= input$ncommittee)
    }
    names(server.env$crt.fits)[crt.n.fits] <- paste(crt.n.fits, "-", input$algo)
    
    # Update models list on the left
    updateSelectInput(session, "fit", choices= rev(names(crt.fits)))
    
    # Update train button counter
    server.env$crt.train.clicks <- input$trainbutton
  }})
  
  # Training message
  output$trainMessage <- renderPrint({
    dInput()
    if (is.null(input$file1))
      return(cat("First import a dataset."))
    input$trainbutton
    if(is.null(input$varchoicein) | is.null(input$varchoiceout)) 
      return(cat("Choose at least one input and one output variable."))
    if (length(crt.fits) == 0) 
      return(cat("Hit the Train button to train the neural network."))
    cat("Training successful.")
  })
  
  # Summary of the SOM
  output$summary <- renderPrint({ 
    dInput()
    if (is.null(input$file1))
      return("First import a dataset.")
    input$trainbutton
    if (length(crt.fits) == 0) 
      return("Hit the Train button to train the neural network.")
    
    tmp.fit <- crt.fits[[input$fit]]
    
    cat(input$trainbutton, crt.train.clicks,
        if (nrow(current.all.data) != nrow(tmp.fit$data)) {
          paste("Warning:", nrow(current.all.data) - nrow(tmp.fit$data), 
                "observations removed because of NA values.\n")
        } else {""},
        paste("Train MSE:", 
              mean((tmp.fit$pred[tmp.fit$train,]-
                      as.matrix(tmp.fit$data[tmp.fit$train,
                                             tmp.fit$namesout]))**2)),'\n',
        paste("Test MSE:", 
              mean((tmp.fit$pred[tmp.fit$test,]-
                      as.matrix(tmp.fit$data[tmp.fit$test, tmp.fit$namesout]))**2)), '\n',
        paste("Committee members training errors\n", 
              paste(sapply(tmp.fit$net, 
                           function(x) switch(tmp.fit$algo,
                                              "mlp"= x$MSE_final,
                                              "elm"= mean(x$residuals**2),
                                              "nnet"= mean(x$residuals**2))),
                    collapse= " "))
    )
    
  })
  
  ##############################################################################
  ## Diagnostics
    
  updateVarDiagno <- function() {
    updateSelectInput(session, "diagvarchoiceout",
                      choices= if (ncol(current.pred) == 1) {
                        colnames(current.matrix)[1]
                      } else current.namesout)
    updateSelectInput(session, "diagvarchoicein",
                      choices= current.datnamesin)
  }
  
  # update available plots in diagnostics
  observe({
    updateSelectInput(session, "diagplottype", 
                      choices= switch(input$algo, 
                                      "mlp"= list("Training error"= "trainerr",
                                                  "Actual vs Fitted"= "actfit",
                                                  "Error distribution"= "errdistr",
                                                  "Residual vs Fitted"= "residfit",
                                                  "Residual vs Predictor"= "residinput"),
                                      list("Actual vs Fitted"= "actfit",
                                           "Error distribution"= "errdistr",
                                           "Residual vs Fitted"= "residfit",
                                           "Residual vs Predictor"= "residinput")))
  })
  
  # Diagnostics plot
  output$diagplot <- renderPlot({
    input$trainbutton
    if(is.null(current.net))
      return(NULL)
    
    tmp.sample <- switch(input$diagsample,
                         "training"= current.train, 
                         "test"= current.test,
                         "whole"= c(current.train, current.test))
    switch(input$diagplottype, 
           "trainerr"= {
             plot(current.net[[1]]$evol_err, ylim= range(c(current.net[[1]]$evol_err, 
                                                           current.net[[1]]$evol_test)),
                  xlab= "iteration", ylab= "training error")
             lines(current.net[[1]]$evol_test, t="p", col= 2)
             legend("topright", col= 1:2, pch= c(1,1), 
                    c("Training sample error", "Test sample error"))
           },
           "actfit"= {
             plot(current.pred[tmp.sample, 
                               if(ncol(current.pred) == 1) {1} else 
                                 (1:ncol(current.pred))[
                                   current.namesout == 
                                     input$predvarchoiceout]],
                  current.data[tmp.sample, input$diagvarchoiceout],
                  xlab= paste("predicted", input$diagvarchoiceout),
                  ylab= paste("observed", input$diagvarchoiceout))
             abline(0,1,col=2,lwd=2,lty=2)
             legend("topleft", col= 2, lwd= 2, lty= 2, "y = x")
           },
           "residfit"= {
             tmp.resid <- current.data[tmp.sample, input$diagvarchoiceout] -
               current.pred[tmp.sample, 
                            if(ncol(current.pred) == 1) {1} else 
                              (1:ncol(current.pred))[
                                current.namesout == 
                                  input$predvarchoiceout]]
             
             plot(current.pred[tmp.sample, 
                               if(ncol(current.pred) == 1) {1} else 
                                 (1:ncol(current.pred))[
                                   current.namesout == 
                                     input$predvarchoiceout]],
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
             tmp.resid <- current.data[tmp.sample, input$diagvarchoiceout] -
               current.pred[tmp.sample, 
                            if(ncol(current.pred) == 1) {1} else 
                              (1:ncol(current.pred))[
                                current.namesout == 
                                  input$predvarchoiceout]]
             plot(current.data[tmp.sample, input$diagvarchoicein], 
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
             tmp.resid <- current.data[tmp.sample, input$diagvarchoiceout] -
               current.pred[tmp.sample, 
                            if(ncol(current.pred) == 1) {1} else 
                              (1:ncol(current.pred))[
                                current.namesout == 
                                  input$predvarchoiceout]]
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
  
  updateVarPred <- function() {
    updateSelectInput(session, "predvarchoicein",
                      choices= input$varchoicein)
    updateSelectInput(session, "predvarchoiceout",
                      choices= if (ncol(current.pred) == 1) {
                        colnames(current.matrix)[1]
                      } else current.namesout)
  }
  
  # plot predictions
  output$predplot <- renderPlot({
    input$trainbutton
    if (is.null(current.net)) return(NULL)
    
    tmp.sample <- switch(input$predsample,
                         "training"= current.train, 
                         "test"= current.test,
                         "whole"= c(current.train, current.test))
    tmp.tab <- data.frame(current.data[tmp.sample, input$predvarchoicein],
                          current.pred[tmp.sample, if(ncol(current.pred) == 1) {1} else 
                            (1:ncol(current.pred))[current.namesout == input$predvarchoiceout]])
    plot(tmp.tab, 
         xlab= input$predvarchoicein, 
         ylab= input$predvarchoiceout,
         ylim= range(current.data[tmp.sample, input$predvarchoiceout]))
    if (input$predspline)
      lines(smooth.spline(tmp.tab, cv=  TRUE), col= 2, lwd= 2)
    if(input$predshowobs == TRUE) {
      lines(current.data[tmp.sample, c(input$predvarchoicein, 
                                       input$predvarchoiceout)],
            col= 3, pch= 2, t= "p")
      if (input$predspline)
        lines(smooth.spline(current.data[tmp.sample, 
                                         c(input$predvarchoicein,
                                           input$predvarchoiceout)], cv= TRUE),
              lty=2, col= 4, lwd= 2)
    }
  })
  
  # Download predictions
  output$preddownload <- {
    downloadHandler(filename= function() {
      paste("mlp_pred_",format(Sys.time(),format="-%Y-%m-%d_%H:%M"),".csv",sep="")
    }, 
                    content= function(file) {
                      write.csv(current.pred, file= file, 
                                row.names= rownames(current.all.data)[rownames(current.all.data)
                                                                      %in% rownames(current.data)],
                                col.names= input$varchoiceout)
                    })
  }
  
  
  ##############################################################################
  ## Partial derivatives
  
  updateVarDer <- function() {
    updateSelectInput(session, "dervarchoicein",
                      choices= input$varchoicein)
    updateSelectInput(session, "dervarchoiceout",
                      choices= if (ncol(current.pred) == 1) {
                        colnames(current.matrix)[1]
                      } else current.namesout)
    updateSelectInput(session, "dervarchoice2order",
                      choices= input$varchoicein)
    
  }
  
  # compute derivatives
  computeDer <- function(algo, dersample, 
                         dervarchoicein, dervarchoiceout, ncommittee,
                         activhid, activout) {
    
    tmp.sample <- switch(dersample,
                         "training"= current.train, 
                         "test"= current.test,
                         "whole"= c(current.train, current.test))
    
    if (algo == "mlp") {
      tmp.der <- 0
      for (i_commi2 in 1:ncommittee) {
        tmp.der <- tmp.der + 
          FNetPartial(le_net= current.net[[i_commi2]], 
                      data_in= current.matrix[tmp.sample, ],
                      id_var= if(ncol(current.pred) == 1) {0} else 
                        (1:length(current.namesout))[
                          current.namesout ==
                            dervarchoiceout] - 1) / 
          ncommittee
        
      }
      tmp.der <- tmp.der * 2 * (crt.varout.sd[which(current.namesout == dervarchoiceout)] / 
                                      (crt.varin.range[2, dervarchoicein] - 
                                         crt.varin.range[1, dervarchoicein]))
    } else if (algo == "nnet") {
      tmp.der <- 0
      matASubset= 1:(current.net[[1]]$n[2] * (1 + current.net[[1]]$n[1]))
      matBSubset= (1 + current.net[[1]]$n[2] * (1 + current.net[[1]]$n[1])):
        length(current.net[[1]]$wts)
      for (i_commi2 in 1:ncommittee) {
        tmp.der <- tmp.der + 
          partialDer(input= current.matrix[tmp.sample, current.matnamesin], 
                     matA= matrix(current.net[[i_commi2]]$wts[matASubset],
                                  ncol= current.net[[i_commi2]]$n[2]),
                     matB= matrix(current.net[[i_commi2]]$wts[matBSubset],
                                  ncol= current.net[[i_commi2]]$n[3]),
                     index.in= which(current.matnamesin == 
                                       dervarchoicein),
                     index.out= which(current.namesout ==
                                        dervarchoiceout),
                     activHid= input$activhid,
                     activOut= input$activout,
                     standard.in= (crt.varin.range[2, dervarchoicein] - 
                                                   crt.varin.range[1, dervarchoicein]) / 2,
                     standard.out= crt.varout.sd[which(current.namesout == dervarchoiceout)]) / 
          ncommittee
      }
    } else if (algo == "elm") {
      tmp.der <- 0
      for (i_commi in 1:ncommittee) {
        tmp.der <- tmp.der + 
          partialDer(input= current.matrix[tmp.sample, current.matnamesin], 
                     matA= as.matrix(rbind(current.net[[i_commi]]$biashid, 
                                           t(current.net[[i_commi]]$inpweight))),
                     matB= rbind(0, current.net[[i_commi]]$outweight),
                     index.in= which(current.matnamesin == 
                                       dervarchoicein),
                     index.out= which(current.namesout ==
                                        dervarchoiceout),
                     activHid= input$activhid,
                     activOut= input$activout,
                     standard.in= (crt.varin.range[2, dervarchoicein] - 
                                     crt.varin.range[1, dervarchoicein]) / 2,
                     standard.out= crt.varout.sd[which(current.namesout == dervarchoiceout)]) / 
          ncommittee
      }
    }
    as.matrix(tmp.der)
  }
  
  # compute derivatives when button is hit
  observe({
    input$trainbutton
    if (is.null(current.net)) return(NULL)
    input$derbutton
    if (input$derbutton > crt.der.clicks) {
      if (input$algo == "nnet" & input$activhid != "logistic") 
        stop("Hidden activation must be logistic")
      if (input$algo == "nnet" & input$activout != "identity") 
        stop("Hidden activation must be identity")
      
      server.env$current.der <- computeDer(input$algo, input$dersample, 
                                           input$dervarchoicein, 
                                           input$dervarchoiceout, 
                                           input$ncommittee, 
                                           input$activhid, 
                                           input$activout)
      server.env$current.der.varin <- input$dervarchoicein
      server.env$current.der.varout <- input$dervarchoiceout
      crt.der.clicks <- input$derbutton
    }
  })
  
  # current derivative 
  output$dertext <- renderPrint({
    input$trainbutton
    if (is.null(current.net)) return("First train net.")
    input$derbutton
    if (is.null(current.der)) 
      return("Click predict button to predict derivatives.")
    
    cat("Current : partial derivative of", current.der.varout,
        "with respect to", current.der.varin,
        "\n(click predict button to change variables)")
    
  })
  
  # plot derivatives
  output$derplot <- renderPlot({
    input$trainbutton
    if (is.null(current.net)) return(NULL)
    input$derbutton
    if (is.null(current.der)) return(NULL)
    
    tmp.sample <- switch(input$dersample,
                         "training"= current.train, 
                         "test"= current.test,
                         "whole"= c(current.train, current.test))
    
    if(!(current.der.varin %in% current.matnamesin))
      stop("Input variable must be numeric.")
    tmp.varin <- ifelse(input$der2order, 
                        input$dervarchoice2order,
                        current.der.varin)
    if (input$algo == "mlp") {
      tmp.tab <- cbind(current.data[tmp.sample, tmp.varin],
                       current.der[, which(colnames(current.der) == 
                                             current.der.varin)])
    } else tmp.tab <- cbind(current.data[tmp.sample, tmp.varin], current.der)
    
    plot(tmp.tab, 
         xlab= tmp.varin, 
         ylab= paste("partial (",current.der.varout,"/",
                     current.der.varin,")"),
         ylim= range(c(0,tmp.tab[,2])))
    tmp.spline= smooth.spline(tmp.tab, cv=  TRUE)
    if (input$derspline)
      lines(tmp.spline$x, tmp.spline$y, col= 2, lwd= 2)
    abline(h=0, col= 4)
    legend("topleft", lty= 1, col= 4, "y=0")
  })
  
  # Download derivatives
  output$derdownload <- downloadHandler(filename= function() {
    paste("mlp_der_", current.der.varout, "_",
          format(Sys.time(),format="-%Y-%m-%d_%H:%M"),".csv",sep="")
  }, content= function(file) {
    write.csv(current.der, file= file, 
              row.names= rownames(current.all.data)[rownames(current.all.data)
                                                    %in% rownames(current.data)],
              col.names= current.matnamesin)
  })
})


