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
  
  res <- as.matrix(derO * rowSums(derH %*% diag(matA[index.in, ]) %*% diag(matB[-1,index.out]) ))
  (standard.out/standard.in) * res
}


# Server
shinyServer(function(input, output, session) {
  # server environment variables
  server.env <- environment() # used to allocate in functions
  current.net <- NULL # this variable will contain the current net
  current.all.data <- NULL # with na
  current.data <- NULL # without the na of input and output variables
  current.train <- NULL
  current.test <- NULL
  current.matrix <- NULL
  current.pred <- NULL

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

  # Update training sample
  updateSamples <- function() {
    server.env$current.train <- sample(1:nrow(current.data), size= input$ntrain)
    server.env$current.test <- (1:nrow(current.data))[-current.train]
  }
  
  # Train the net when the button is hit
  theTrain<- function() {
    input$trainbutton
    server.env$current.net <- isolate({
      # remove from working data the obs where input or output are NA
      server.env$current.data <- server.env$current.all.data[
        rowSums(is.na(current.all.data[,c(input$varchoicein, 
                                          input$varchoiceout)])) == 0, ]
      
      set.seed(input$randseed)
      updateSamples()
      tmp.matrix <- model.matrix(as.formula(object= paste("~ 0+", 
                                                  paste(input$varchoicein, 
                                                        collapse= "+") )),
                                 data= current.data)
      tmp.rownames <- rownames(tmp.matrix)
      noms.in <- colnames(tmp.matrix)
      tmp.matrix <- as.matrix(cbind(current.data[rownames(tmp.matrix),
                                                 input$varchoiceout], 
                                    tmp.matrix))
      colnames(tmp.matrix) <- c(input$varchoiceout, noms.in)
      rownames(tmp.matrix) <- tmp.rownames
#       tmp.matout <- model.matrix(as.formula(paste("~ 0 +", paste(input$varchoiceout, collapse= "+"))),
#                                  data= current.data)
#       server.env$current.matnamesout <- colnames(tmp.matout)
#       tmp.matrix <- cbind(tmp.matout, tmp.matrix)

      server.env$current.matrix <- tmp.matrix
      server.env$current.matnamesin <- noms.in
      server.env$current.datnamesin <- input$varchoicein
      server.env$current.namesout <- input$varchoiceout
      
      # normalize
      server.env$crt.varin.range <- 
        sapply(as.data.frame(tmp.matrix[current.train, noms.in]), FUN= range)
      for (i_var in current.matnamesin)
        tmp.matrix[,i_var] <- 2 * ( (tmp.matrix[, i_var] - crt.varin.range[1, i_var] ) /
                                      (crt.varin.range[2, i_var] - crt.varin.range[1, i_var]) ) - 1
      server.env$crt.varout.mean <- 
        sapply(as.data.frame(tmp.matrix[current.train, current.namesout]), FUN= mean)
      names(crt.varout.mean) <- current.namesout
      server.env$crt.varout.sd <- 
        sapply(as.data.frame(tmp.matrix[current.train, current.namesout]), FUN= sd)
      names(crt.varout.sd) <- current.namesout
      if (input$activout != "softmax") for (i_var in current.namesout)
        tmp.matrix[, i_var] <- (tmp.matrix[, i_var] - crt.varout.mean[i_var]) / crt.varout.sd[i_var]
      
      tmp.net <- trainTheNet(tmp.matrix, noms.in= noms.in, 
                             noms.out= input$varchoiceout, 
                             hidden= c(input$nhid1, input$nhid2, 
                                       input$nhid3, 
                                       input$nhid4)[1:input$nhidlay],
                             niter= input$maxit, 
                             activ.hid= input$activhid, 
                             activ.out= input$activout,
                             rand.seed= input$randseed, train= current.train, 
                             test= current.test, regul= input$regul,
                             ncommit= input$ncommittee, algo= input$algo)
      

      # predict
      server.env$current.pred <- matrix(0, ncol= length(input$varchoiceout),
                                        nrow= nrow(current.matrix))
      for (i_commi in 1:input$ncommittee)
        server.env$current.pred <- server.env$current.pred + predictTheNet(
          net= tmp.net[[i_commi]], newdata= tmp.matrix, algo= input$algo, 
          noms.in= current.matnamesin) / input$ncommittee
      
      # reverse normalization of prediction
       if (input$activout != "softmax") for (i_var in 1:ncol(current.pred))
         server.env$current.pred[, i_var] <- (server.env$current.pred[, i_var] * 
                                                crt.varout.sd[i_var]) + crt.varout.mean[i_var]
      
      tmp.net
    })
    
    # Update variables and launch plots
    updateVarDiagno()
    plotDiagno()
    updateVarPred() 
    plotPred()
    updateVarDer()
    plotDer()
    
    # return the computed net
    server.env$current.net
  }
  
  # Render the summary of the SOM
  output$summary <- renderPrint({ 
    dInput()
    if (is.null(input$file1))
      return("First import a dataset.")
    if (input$trainbutton==0) 
      return("Hit the Train button to train the neural network.")
    
    theTrain()

    cat(
      if (nrow(current.all.data) != nrow(current.data)) {
        paste("Warning:", nrow(current.all.data) - nrow(current.data), 
              "observations removed because of NA values.\n")
      } else {""},
     paste("Train MSE:", 
           mean((current.pred[current.train,]-
                   as.matrix(current.data[current.train, input$varchoiceout]))**2)),'\n',
     paste("Test MSE:", 
           mean((current.pred[current.test,]-
                   as.matrix(current.data[current.test, input$varchoiceout]))**2)), '\n',
    paste("Committee members training errors\n", 
            paste(sapply(current.net, 
                         function(x) switch(input$algo,
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

  # Diagnostics plot
  plotDiagno= function() observe({
    if(input$trainbutton==0)
      return(NULL)
    if(is.null(current.net))
      return(NULL)
    
    output$diagplot <- renderPlot({
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
  plotPred= function() observe({
    if(input$predbutton==0)
      return(NULL)
    
    output$predplot <- renderPlot({
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
    output$preddownload <- downloadHandler(filename= function() {
      paste("mlp_pred_",format(Sys.time(),format="-%Y-%m-%d_%H:%M"),".csv",sep="")
    }, 
                                           content= function(file) {
      write.csv(current.pred, file= file, 
                row.names= rownames(current.all.data)[rownames(current.all.data)
                                                      %in% rownames(current.data)],
                col.names= input$varchoiceout)
    })
  })
  
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
      tmp.der <- FNetPartial(le_net= current.net[[1]], 
                             data_in= current.matrix[tmp.sample, ],
                             id_var= if(ncol(current.pred) == 1) {0} else 
                               (1:length(current.namesout))[
                                 current.namesout ==
                                   dervarchoiceout] - 1)
      tmp.der <- tmp.der / ncommittee
      if (ncommittee >= 2) for (i_commi2 in 2:ncommittee) {
        tmp.der <- tmp.der + 
          FNetPartial(le_net= current.net[[i_commi2]], 
                      data_in= current.matrix[tmp.sample, ],
                      id_var= if(ncol(current.pred) == 1) {0} else 
                        (1:length(current.namesout))[
                          current.namesout ==
                            dervarchoiceout] - 1) / 
          ncommittee
        
      }
      tmp.der <- tmp.der * (crt.varout.sd[which(current.namesout == dervarchoiceout)] / 
                              (crt.varin.range[2, dervarchoicein] - 
                                 crt.varin.range[1, dervarchoicein]))
    } else if (algo == "nnet") {
      matASubset= 1:(current.net[[1]]$n[2] * (1 + current.net[[1]]$n[1]))
      matBSubset= (1 + current.net[[1]]$n[2] * (1 + current.net[[1]]$n[1])):
        length(current.net[[1]]$wts)
      tmp.der <- partialDer(input= current.matrix[tmp.sample, current.matnamesin], 
                            matA= matrix(current.net[[1]]$wts[matASubset],
                                         ncol= current.net[[1]]$n[2]),
                            matB= matrix(current.net[[1]]$wts[matBSubset],
                                         ncol= current.net[[1]]$n[3]),
                            index.in= which(current.matnamesin == 
                                              dervarchoicein),
                            index.out= which(current.namesout ==
                                               dervarchoiceout),
                            activHid= activhid,
                            activOut= activout,
                            standard.in= crt.varin.range[2, dervarchoicein] - 
                              crt.varin.range[1, dervarchoicein],
                            standard.out= crt.varout.sd[which(current.namesout == dervarchoiceout)])
      tmp.der <- tmp.der / ncommittee
      
      if (ncommittee >= 2) for (i_commi2 in 2:ncommittee) {
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
                     standard.in= crt.varin.range[2, dervarchoicein] - 
                       crt.varin.range[1, dervarchoiceout],
                     standard.out= crt.varout.sd[which(current.namesout == dervarchoiceout)]) / 
          ncommittee
      }
    }
    as.matrix(tmp.der)
  }
  
  # plot derivatives
  plotDer= function() observe({
    if(input$derbutton==0)
      return(NULL)
    
    if (input$algo == "nnet" & input$activhid != "logistic") 
      stop("Hidden activation must be logistic")
    if (input$algo == "nnet" & input$activout != "identity") 
      stop("Hidden activation must be identity")

    tmp.der <- computeDer(input$algo, input$dersample, 
                          input$dervarchoicein, input$dervarchoiceout, 
                          input$ncommittee, input$activhid, 
                          input$activout)
    tmp.sample <- switch(input$dersample,
                         "training"= current.train, 
                         "test"= current.test,
                         "whole"= c(current.train, current.test))
    
    output$derplot <- renderPlot({
      if(!(input$dervarchoicein %in% current.matnamesin))
        stop("Input variable must be numeric.")
      tmp.varin <- ifelse(input$der2order, 
                          input$dervarchoice2order,
                          input$dervarchoicein)
      if (input$algo == "mlp") {
        tmp.tab <- cbind(current.data[tmp.sample, tmp.varin],
                         tmp.der[, (1:ncol(tmp.der))[
                           colnames(tmp.der) == input$dervarchoicein]])
      } else tmp.tab <- cbind(current.data[tmp.sample, tmp.varin], tmp.der)
      
      plot(tmp.tab, 
           xlab= tmp.varin, 
           ylab= paste("partial (",input$dervarchoiceout,"/",
                       input$dervarchoicein,")"),
           ylim= range(c(0,tmp.tab[,2])))
      tmp.spline= smooth.spline(tmp.tab, cv=  TRUE)
      if (input$derspline)
        lines(tmp.spline$x, tmp.spline$y, col= 2, lwd= 2)
      abline(h=0, col= 4)
      legend("topleft", lty= 1, col= 4, "y=0")
    })
  })
    
  # Download derivatives
  output$derdownload <- downloadHandler(filename= function() {
    paste("mlp_der_", input$dervarchoiceout, "_",
          format(Sys.time(),format="-%Y-%m-%d_%H:%M"),".csv",sep="")
  }, content= function(file) {
    tmp.der <- computeDer(input$algo, input$dersample, 
                          input$dervarchoicein, input$dervarchoiceout, 
                          input$ncommittee, input$activhid,
                          input$activout)
    write.csv(tmp.der, file= file, 
              row.names= rownames(current.all.data)[rownames(current.all.data)
                                                    %in% rownames(current.data)],
              col.names= current.matnamesin)
  })
})


