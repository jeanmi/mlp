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

################################################

dat= read.csv("~/code/SwarmNet shiny/sqr(x1)_plus_x2x3_plus_x4.csv")
dat_orig= dat
input_names= paste("x", 1:4, sep= "")
train= sample(1:nrow(dat), 800)
test= (1:nrow(dat))[-train]

ranges= NULL
for (i in input_names) {
	ranges= cbind(ranges, c(min(dat[,i]), max(dat[,i])))
	dat[,i]= 2*(dat[,i] - min(dat[,i]))/(max(dat[,i])-min(dat[,i])) - 1
}
colnames(ranges)= input_names
mean_y= mean(dat$y)
sd_y= sd(dat$y)
dat$y= (dat$y - mean_y)/ sd_y

X= as.matrix(dat[, input_names])

## nnet
library(nnet)
mlp <- nnet(x= X[train,], y= dat$y[train], size= 5, linout= TRUE, maxit= 100)
pred <- predict(mlp, dat)
pred <- pred * sd_y + mean_y
mean((pred[train] - dat_orig[train,"y"])**2)
mean((pred[test] - dat_orig[test,"y"])**2)

matASubset= 1:(mlp$n[2] * (1 + mlp$n[1]))
matBSubset= (1 + mlp$n[2] * (1 + mlp$n[1])):length(mlp$wts)

la_in= "x1"
der <- partialDer(input= dat[, input_names], 
                            matA= matrix(mlp$wts[matASubset],
                                         ncol= mlp$n[2]),
                            matB= matrix(mlp$wts[matBSubset],
                                         ncol= mlp$n[3]),
                            index.in= which(input_names == la_in),
                            index.out= 1,
                            activHid= "logistic",
                            activOut= "identity",
                            standard.in= (ranges[2, la_in] - ranges[1, la_in]) / 2,
                            standard.out= sd_y)
plot(dat_orig[,la_in], der)

## elm
library(elmNN)
elm <- elmtrain(x= X[train, ], y= dat$y[train], nhid= 100, actfun= "sig")

pred <- predict(elm, dat[,input_names])
pred <- pred * sd_y + mean_y
mean((pred[train] - dat_orig[train,"y"])**2)
mean((pred[test] - dat_orig[test,"y"])**2)

la_in= "x4"
der <- partialDer(input= dat[, input_names], 
                            matA= as.matrix(rbind(elm$biashid, t(elm$inpweight))),
                            matB= rbind(0, elm$outweight),
                            index.in= which(input_names == la_in),
                            index.out= 1,
                            activHid= "logistic",
                            activOut= "identity",
                            standard.in= (ranges[2, la_in] - ranges[1, la_in]) / 2,
                            standard.out= sd_y)
plot(dat_orig[,la_in], der)

