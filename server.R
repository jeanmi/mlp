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
  } else stop("wrong activOut")
  
  inputH <- as.matrix(cbind(1, input)) %*% matA
  matH <- actHidFun(inputH)
  derH <- actHidDer(inputH)
    
  inputO <- as.matrix(cbind(1, matH)) %*% matB
  derO <- actOutDer(inputO)
  
  res <- as.matrix(derO[, index.out] * rowSums(derH %*% diag(matA[index.in + 1, ] * matB[-1, index.out]) ))
  (standard.out/standard.in) * res
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
  chosen.fit <- NULL # current net, taken from crt.fits
  
  ##############################################################################
  ## Left panel
  
  # Store current fit in chosen.fit
  observe({
    input$fit # (fit selector is updated at the end of each training)
    if(crt.n.fits == 0) return(NULL)
    server.env$chosen.fit <- crt.fits[[input$fit]]
  })
  
  # Summary of current fit
  output$summary <- renderPrint({ 
    input$fit
    if(crt.n.fits == 0) return(NULL)
    
    cat(input$trainbutton, crt.train.clicks,
        if (nrow(current.all.data) != nrow(chosen.fit$data)) {
          paste("Warning:", nrow(current.all.data) - nrow(chosen.fit$data), 
                "observations removed because of NA values.\n")
        } else {""},
        paste("Train MSE:", 
              mean((chosen.fit$pred[chosen.fit$train,]-
                      as.matrix(chosen.fit$data[chosen.fit$train,
                                             chosen.fit$namesout]))**2)),'\n',
        paste("Test MSE:", 
              mean((chosen.fit$pred[chosen.fit$test,]-
                      as.matrix(chosen.fit$data[chosen.fit$test, chosen.fit$namesout]))**2)), '\n',
        paste("Committee members training errors\n", 
              paste(sapply(chosen.fit$net, 
                           function(x) switch(chosen.fit$algo,
                                              "mlp"= x$MSE_final,
                                              "elm"= mean(x$residuals**2),
                                              "nnet"= mean(x$residuals**2))),
                    collapse= " "))
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
                                   sep=input$sep, quote=input$quote, dec=input$dec)
    
    # update the "input variables" checkbox
    updateVarChoiceIn()
    updateVarChoiceOut()
    updateTrainSlider()
    
    # clear trained networks
    server.env$crt.fits <- list()
    server.env$crt.n.fits <- 0
    server.env$chosen.fit <- NULL
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
    
    # normalize (TODO : fix problem when only one input)
    tmp.varin.range <- as.matrix(sapply(as.data.frame(tmp.matrix[tmp.train, tmp.matnamesin]), FUN= range))
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
    } else tmp.hidden <- input$nhid1
    
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
           ncommittee= input$ncommittee,
           activhid= input$activhid,
           activout= input$activout,
           randseed= input$randseed)
    }
    names(server.env$crt.fits)[crt.n.fits] <- paste(crt.n.fits, "-", input$algo)
    
    # Update models list on the left
    updateSelectInput(session, "fit", choices= rev(names(crt.fits)))
    
    # Update train button counter
    server.env$crt.train.clicks <- input$trainbutton
  }})
  
  # Training message
  output$trainMessage <- renderPrint({
    input$fit # (fit selector is updated at the end of each training)
    dInput()
    if (is.null(input$file1))
      return(cat("First import a dataset."))
    input$trainbutton
    if(is.null(input$varchoicein) | is.null(input$varchoiceout)) 
      return(cat("Choose at least one input and one output variable."))
    if (crt.n.fits == 0) 
      return(cat("Hit the Train button to train the neural network."))
    cat(" Training successful. (Name:", names(crt.fits)[crt.n.fits], ")\n",
        "You may train another neural network to compare results.")
  })
  
  
  ##############################################################################
  ## Diagnostics
    
  # update available variables and plots in diagnostics
  # TODO : update available plots with all.diag.plots list
  updateDiagOptions= reactive({
    input$fit
    
    updateSelectInput(session, "diagvarchoiceout",
                      choices= chosen.fit$namesout,
                      selected= if (input$diagvarchoiceout %in% chosen.fit$namesout) {
                        input$diagvarchoiceout
                      } else 1)
    updateSelectInput(session, "diagvarchoicein",
                      choices= chosen.fit$datnamesin,
                      selected= if (input$diagvarchoicein %in% chosen.fit$datnamesin) {
                        input$diagvarchoicein
                      } else 1)
  })
  
  # Diagnostics plot
  output$diagplot <- renderPlot({
    input$fit
    if (crt.n.fits == 0) return(NULL)

    updateDiagOptions()
    output$diagMessage <- renderText("")
    
    tmp.sample <- switch(input$diagsample,
                         "training"= chosen.fit$train, 
                         "test"= chosen.fit$test,
                         "whole"= c(chosen.fit$train, chosen.fit$test))
    switch(input$diagplottype, 
           "trainerr"= {
             if (chosen.fit$algo != "mlp") {
               output$diagMessage <- 
                 renderText("Plot only available for 'mlp' fits.")
               return(NULL)
             } else output$diagMessage <- renderText("")
             plot(chosen.fit$net[[1]]$evol_err, 
                  ylim= range(c(chosen.fit$net[[1]]$evol_err, 
                                chosen.fit$net[[1]]$evol_test)),
                  xlab= "iteration", ylab= "training error")
             lines(chosen.fit$net[[1]]$evol_test, t="p", col= 2)
             legend("topright", col= 1:2, pch= c(1,1), 
                    c("Training sample error", "Test sample error"))
           },
           "actfit"= {
             plot(chosen.fit$pred[tmp.sample, 
                               if(ncol(chosen.fit$pred) == 1) {1} else 
                                 which(chosen.fit$namesout == 
                                         input$diagvarchoiceout)],
                  chosen.fit$data[tmp.sample, input$diagvarchoiceout],
                  xlab= paste("predicted", input$diagvarchoiceout),
                  ylab= paste("observed", input$diagvarchoiceout))
             abline(0,1,col=2,lwd=2,lty=2)
             legend("topleft", col= 2, lwd= 2, lty= 2, "y = x")
           },
           "residfit"= {
             tmp.resid <- chosen.fit$data[tmp.sample, input$diagvarchoiceout] -
               chosen.fit$pred[tmp.sample, 
                               if(ncol(chosen.fit$pred) == 1) {1} else 
                                 which(chosen.fit$namesout == 
                                         input$diagvarchoiceout)]
                               
             plot(chosen.fit$pred[tmp.sample, 
                               if(ncol(chosen.fit$pred) == 1) {1} else 
                                 (1:ncol(chosen.fit$pred))[
                                   chosen.fit$namesout == 
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
             tmp.resid <- chosen.fit$data[tmp.sample, input$diagvarchoiceout] -
               chosen.fit$pred[tmp.sample, 
                            if(ncol(chosen.fit$pred) == 1) {1} else 
                              which(chosen.fit$namesout == 
                                      input$diagvarchoiceout)]
             plot(chosen.fit$data[tmp.sample, input$diagvarchoicein], 
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
             tmp.resid <- chosen.fit$data[tmp.sample, input$diagvarchoiceout] -
               chosen.fit$pred[tmp.sample, 
                            if(ncol(chosen.fit$pred) == 1) {1} else 
                              which(chosen.fit$namesout == 
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
                      choices= chosen.fit$datnamesin,
                      selected= if (input$predvarchoicein %in% chosen.fit$datnamesin) {
                        input$predvarchoicein
                      } else 1)
    updateSelectInput(session, "predvarchoiceout",
                      choices= chosen.fit$namesout,
                      selected= if (input$predvarchoiceout %in% chosen.fit$namesout) {
                        input$predvarchoiceout
                      } else 1)
  })
  
  # plot predictions
  output$predplot <- renderPlot({
    input$fit
    if (crt.n.fits == 0) return(NULL)
    updateVarPred()
    
    tmp.sample <- switch(input$predsample,
                         "training"= chosen.fit$train, 
                         "test"= chosen.fit$test,
                         "whole"= c(chosen.fit$train, chosen.fit$test))
    tmp.tab <- data.frame(chosen.fit$data[tmp.sample, input$predvarchoicein],
                          chosen.fit$pred[tmp.sample, if(ncol(chosen.fit$pred) == 1) {1} else 
                            (1:ncol(chosen.fit$pred))[chosen.fit$namesout == input$predvarchoiceout]])
    plot(tmp.tab, 
         xlab= input$predvarchoicein, 
         ylab= input$predvarchoiceout,
         ylim= range(chosen.fit$data[tmp.sample, input$predvarchoiceout]))

    if (input$predsmooth) {
      tmp.lowess <- lowess(x= tmp.tab[, 1], y= tmp.tab[, 2], 
                           f= ifelse(input$predLowessAlpha > 0, 
                                     input$predLowessAlpha, 1e-4))
      lines(cbind(tmp.lowess$x, tmp.lowess$y)[order(tmp.lowess$x),], 
            col= 2, lwd= 2)
    }
    if(input$predshowobs == TRUE) {
      lines(chosen.fit$data[tmp.sample, c(input$predvarchoicein, 
                                          input$predvarchoiceout)],
            col= 3, pch= 2, t= "p")
      if (input$predsmooth) {
        tmp.lowess <- lowess(x= chosen.fit$data[tmp.sample, input$predvarchoicein],
                             y= chosen.fit$data[tmp.sample, input$predvarchoiceout],
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
      paste("mlp_pred_",format(Sys.time(),format="-%Y-%m-%d_%H:%M"),".csv",sep="")
    }, 
                    content= function(file) {
                      write.csv(chosen.fit$pred, file= file, 
                                row.names= rownames(current.all.data)[rownames(current.all.data)
                                                                      %in% rownames(chosen.fit$data)],
                                col.names= chosen.fit$namesout)
                    })
  }
  
  
  ##############################################################################
  ## Partial derivatives
  # TODO : categorical inputs
  
  updateVarDer <- reactive({
    input$fit
    updateSelectInput(session, "dervarchoicein",
                      choices= chosen.fit$matnamesin[chosen.fit$matnamesin %in% 
                                                       chosen.fit$datnamesin],
                      selected= if (input$dervarchoicein %in% 
                                      chosen.fit$matnamesin[chosen.fit$matnamesin %in% 
                                                              chosen.fit$datnamesin]) {
                        input$dervarchoicein
                      } else 1)
    updateSelectInput(session, "dervarchoiceout",
                      choices= chosen.fit$namesout,
                      selected= if (input$dervarchoiceout %in% chosen.fit$namesout) {
                        input$dervarchoiceout
                      } else 1)
    updateSelectInput(session, "dervarchoice2order",
                      choices= chosen.fit$datnamesin,
                      selected= if (input$dervarchoice2order %in% chosen.fit$datnamesin) {
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
          FNetPartial(le_net= chosen.fit$net[[i_commi2]], 
                      data_in= chosen.fit$matrix,
                      id_var= if(ncol(chosen.fit$pred) == 1) {0} else 
                        (1:length(chosen.fit$namesout))[
                          chosen.fit$namesout ==
                            dervarchoiceout] - 1) / 
          ncommittee
        
      }
      for (i_var in chosen.fit$matnamesin)
        tmp.der[, i_var] <- tmp.der[, i_var] * 2 * 
        (chosen.fit$varout.sd[which(chosen.fit$namesout == dervarchoiceout)] / 
           (chosen.fit$varin.range[2, i_var] - chosen.fit$varin.range[1, i_var]))
    } else if (algo == "nnet") {
      tmp.der <- 0
      matASubset= 1:(chosen.fit$net[[1]]$n[2] * (1 + chosen.fit$net[[1]]$n[1]))
      matBSubset= (1 + chosen.fit$net[[1]]$n[2] * (1 + chosen.fit$net[[1]]$n[1])):
        length(chosen.fit$net[[1]]$wts)
      for (i_commi2 in 1:ncommittee) {
        tmp.der <- tmp.der + 
          partialDer(input= chosen.fit$matrix[, chosen.fit$matnamesin], 
                     matA= matrix(chosen.fit$net[[i_commi2]]$wts[matASubset],
                                  ncol= chosen.fit$net[[i_commi2]]$n[2]),
                     matB= matrix(chosen.fit$net[[i_commi2]]$wts[matBSubset],
                                  ncol= chosen.fit$net[[i_commi2]]$n[3]),
                     index.in= which(chosen.fit$matnamesin == 
                                       dervarchoicein),
                     index.out= which(chosen.fit$namesout ==
                                        dervarchoiceout),
                     activHid= input$activhid,
                     activOut= input$activout,
                     standard.in= (chosen.fit$varin.range[2, dervarchoicein] - 
                                                   chosen.fit$varin.range[1, dervarchoicein]) / 2,
                     standard.out= chosen.fit$varout.sd[which(chosen.fit$namesout == dervarchoiceout)]) / 
          ncommittee
      }
    } else if (algo == "elm") {
      tmp.der <- 0
      for (i_commi in 1:ncommittee) {
        tmp.der <- tmp.der + 
          partialDer(input= chosen.fit$matrix[, chosen.fit$matnamesin], 
                     matA= as.matrix(rbind(chosen.fit$net[[i_commi]]$biashid, 
                                           t(chosen.fit$net[[i_commi]]$inpweight))),
                     matB= rbind(0, chosen.fit$net[[i_commi]]$outweight),
                     index.in= which(chosen.fit$matnamesin == dervarchoicein),
                     index.out= which(chosen.fit$namesout == dervarchoiceout),
                     activHid= input$activhid,
                     activOut= input$activout,
                     standard.in= (chosen.fit$varin.range[2, dervarchoicein] - 
                                     chosen.fit$varin.range[1, dervarchoicein]) / 2,
                     standard.out= chosen.fit$varout.sd[which(chosen.fit$namesout == dervarchoiceout)]) / 
          ncommittee
      }
    }
    as.matrix(tmp.der)
  }
  
  # compute derivatives when button is hit
  observe({
    input$fit
    if (crt.n.fits == 0) return(NULL)
    input$derbutton
    if (!is.null(chosen.fit$der)) {
      server.env$crt.der.clicks <- input$derbutton
      return(NULL)
    }
    if (input$derbutton > crt.der.clicks) {
      for (i_varout in chosen.fit$namesout) {
        tmp.der <- NULL
        if (chosen.fit$algo == "mlp") {
          tmp.der <- computeDer(chosen.fit$algo, 
                                NULL, 
                                i_varout, 
                                chosen.fit$ncommittee, 
                                chosen.fit$activhid, 
                                chosen.fit$activout)
        } else {
          for (i_varin in chosen.fit$matnamesin[chosen.fit$matnamesin %in% 
                                                  chosen.fit$datnamesin]){
            tmp.der <- cbind(tmp.der, computeDer(chosen.fit$algo, 
                                                 i_varin, 
                                                 i_varout, 
                                                 chosen.fit$ncommittee, 
                                                 chosen.fit$activhid, 
                                                 chosen.fit$activout))
          }
          tmp.der <- as.matrix(tmp.der)
          colnames(tmp.der) <- chosen.fit$matnamesin[chosen.fit$matnamesin %in% chosen.fit$datnamesin]
        }
        server.env$chosen.fit$der[[i_varout]] <- tmp.der
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
    if (is.null(chosen.fit$der)) 
      return(cat("Click compute button to predict derivatives."))
    
    cat("Partial derivatives of fit [", input$fit, "] successfully computed.")
    
  })
  
  # plot derivatives
  output$derplot <- renderPlot({
    input$fit
    if (crt.n.fits == 0) return(NULL)
    input$derbutton
    if (is.null(chosen.fit$der)) return(NULL)
    
    tmp.sample <- switch(input$dersample,
                         "training"= chosen.fit$train, 
                         "test"= chosen.fit$test,
                         "whole"= c(chosen.fit$train, chosen.fit$test))
    
    tmp.varin <- ifelse(input$der2order, 
                        input$dervarchoice2order,
                        input$dervarchoicein)
    
    plot(chosen.fit$data[tmp.sample, tmp.varin],
         chosen.fit$der[[input$dervarchoiceout]][tmp.sample, input$dervarchoicein],
         xlab= tmp.varin, 
         ylab= paste("partial (", input$dervarchoiceout, "/", input$dervarchoicein,")"),
         ylim= range(c(0, chosen.fit$der[[input$dervarchoiceout]][tmp.sample, input$dervarchoicein])))
    
    if (input$dersmooth) {
      tmp.lowess <- lowess(x= chosen.fit$data[tmp.sample, tmp.varin], 
                           y= chosen.fit$der[[input$dervarchoiceout]][tmp.sample, input$dervarchoicein],
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
#     paste("mlp_der_", chosen.fit$der.varout, "_",
#           format(Sys.time(),format="-%Y-%m-%d_%H:%M"),".csv",sep="")
#   }, content= function(file) {
#     write.csv(chosen.fit$der, file= file, 
#               row.names= rownames(current.all.data)[rownames(current.all.data)
#                                                     %in% rownames(chosen.fit$data)],
#               col.names= chosen.fit$matnamesin)
#   })
})


