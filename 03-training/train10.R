#####################################################################################
#HOC2 - M Competitions
#####################################################################################
library(forecast)

#Input data
#####################################################################################
setwd("D:/@Local/R Scripts/HOC2/M4")
load("M4Data.RData")
Mseries <- Filter(function(l) l$period == "Yearly", M4)

setwd("D:/@Local/R Scripts/HOC2/M4/year/clustering")
load("clusterLog.RData")
#clusterCount = number of series per cluster;
#clusterList = list with as many elements as the total number of series:
#clusterList$seriesId = series ID;
#clusterList$clusterId = cluster ID, 1:18 (7, 8 and 9 always empty).

setwd("D:/@Local/R Scripts/HOC2/M4/year/training")
pTrain <- 10 #Percentage of training series
maxorigins <- 10 #Maximum number of rolling origins
minorigins <- 5 #Minimum number of rolling origins
mytype <- 1 #Error type = SMAPE
inicluster <- 10 #Initial training cluster
fincluster <- 10 #Final training cluster

#Output files
#####################################################################################
fileTrain <- "trainLog10.RData"

#Auxiliary functions
#####################################################################################
### Evaluation functions
smape_cal <- function(outsample, forecasts){
  #Used to estimate sMAPE
  outsample <- as.numeric(outsample) ; forecasts<-as.numeric(forecasts)
  smape <- (abs(outsample-forecasts)*200)/(abs(outsample)+abs(forecasts))
  return(smape)
}
mase_cal <- function(insample, outsample, forecasts){
  #Used to estimate MASE
  frq <- frequency(insample)
  forecastsNaiveSD <- rep(NA,frq)
  for (j in (frq+1):length(insample)){
    forecastsNaiveSD <- c(forecastsNaiveSD, insample[j-frq])
  }
  masep<-mean(abs(insample-forecastsNaiveSD),na.rm = TRUE)
  
  outsample <- as.numeric(outsample) ; forecasts <- as.numeric(forecasts)
  mase <- (abs(outsample-forecasts))/masep
  return(mase)
}
### Convex weights function
wConvex5 <- function(y, h, p=5, type=1) {
  #INPUTS:
  # - y - Object of time-series class (ts);
  # - h - The number of predictions forward of each origin;
  # - p - The number of different origins (different validation sets):
  #       a) Default is p=5;
  #       b) maxorigin = length(y)-h;
  #       c) minorigin = max(h, length(y)-h-p+1);
  #       d) norigins = maxorigin - minorigin + 1;
  #       e) If norigins < 1, series is too small!
  # - type - Error type: 1 = SMAPE, 2 = MASE, 3 = Both.
  #The origins varies by steps of one from length(y)-h to minorigin. 
  #The function considers the following (5) methods from the forecast package:
  # 1) ETS;
  # 2) AUTOARIMA;
  # 3) THETA;
  # 4) TBATS;
  # 5) SNAIVE.
  # nmethods = 5.
  #
  #OUTPUTS:
  # - wConvex$W - Matrix [h,nmethods], with HORIZON-OPTIMIZED weights by relative performance (1/error);
  # - wConvex$Ws - Matrix [h,nmethods], with STATIC weights by relative performance;   
  # - wConvex$Fh - Array [h,nmethods,norigins], with in-sample forecasts for each origin;
  # - wConvex$G1 - Array [h,nmethods,norigins], with in-sample SMAPE errors for each origin;
  # - wConvex$G2 - Array [h,nmethods,norigins], with in-sample MASE errors for each origin
  # - wConvex$Yh - Matrix [h,norigins], with in-sample validation sets for each origin.
  out <- tryCatch(
    {
      if (!is.ts(y)) {
        #y <- as.ts(y)
        stop("y must be a TS object!")
      }
      h <- as.integer(h); p <- as.integer(p)
      nmethods <- 5
      maxy = length(y)
      maxorigin = maxy-h
      minorigin = max(h, maxy-h-p+1)
      norigins = maxorigin - minorigin + 1
      if (norigins < 1) {
        stop("Series is too small!")
      }
      fArray <- array(dim = c(h,nmethods,norigins))
      g1Array <- array(dim = c(h,nmethods,norigins))
      g2Array <- array(dim = c(h,nmethods,norigins))
      yVal <- matrix(nrow = h, ncol = norigins)
      wConvex <- list(W = matrix(nrow = h, ncol = nmethods),
                      Ws = matrix(nrow = h, ncol = nmethods),
                      Fh = array(dim = c(h,nmethods,norigins)), 
                      G1 = array(dim = c(h,nmethods,norigins)),
                      G2 = array(dim = c(h,nmethods,norigins)),
                      Yh = matrix(nrow = h, ncol = norigins))
      index = 0
      for (i in maxorigin:minorigin) {
        index = index+1
        #In-sample training and validation sets
        yTrain <- subset(y, start = 1, end = i) 
        yVal[,index] = subset(y, start = (i+1), end = (i+h))
        #In-sample forecasts & errors
        f1 <- forecast(ets(yTrain), h = h, level = 95)
        f2 <- forecast(auto.arima(yTrain), h = h, level = 95)
        f3 <- thetaf(yTrain, h = h, level = 95)
        f4 <- forecast(tbats(yTrain), h = h, level = 95)
        f5 <- snaive(yTrain, h = h, level = 95)
        smape1 <- smape_cal(yVal[,index], f1$mean)
        smape2 <- smape_cal(yVal[,index], f2$mean)
        smape3 <- smape_cal(yVal[,index], f3$mean)
        smape4 <- smape_cal(yVal[,index], f4$mean)
        smape5 <- smape_cal(yVal[,index], f5$mean)
        mase1 <- mase_cal(yTrain, yVal[,index], f1$mean)
        mase2 <- mase_cal(yTrain, yVal[,index], f2$mean)
        mase3 <- mase_cal(yTrain, yVal[,index], f3$mean)
        mase4 <- mase_cal(yTrain, yVal[,index], f4$mean)
        mase5 <- mase_cal(yTrain, yVal[,index], f5$mean)
        fArray[,,index] <- cbind(f1$mean,f2$mean,f3$mean,f4$mean,f5$mean)
        g1Array[,,index] <- cbind(smape1,smape2,smape3,smape4,smape5)
        g2Array[,,index] <- cbind(mase1,mase2,mase3,mase4,mase5)
      }
      g1Array[g1Array==0]=1e-12  #Avoid zero division
      g2Array[g2Array==0]=1e-12  #Avoid zero division
      wConvex$Fh <- fArray 
      wConvex$G1 <- g1Array
      wConvex$G2 <- g2Array
      wConvex$Yh <- yVal
      foo1 <- 1/(rowMeans(colMeans(wConvex$G1))) #static SMAPE inverses
      foo3 <- 1/(rowMeans(colMeans(wConvex$G2))) #static MASE inverses
      foo2 <- 1/(t(colMeans(aperm(wConvex$G1, c(3,2,1))))) #horizon-optimized SMAPE inverses
      foo4 <- 1/(t(colMeans(aperm(wConvex$G2, c(3,2,1))))) #horizon-optimized MASE inverses
      if (type == 2) #MASE
      {
        wConvex$Ws <- matrix(foo3/sum(foo3),nrow = h, ncol = nmethods, byrow = TRUE)
        wConvex$W <- foo4/rowSums(foo4)
      }
      else if (type == 3) #Both
      {
        #The mean of convex matrices are also convex...
        wConvex$Ws <- matrix(0.5*(foo1/sum(foo1)+foo3/sum(foo3)), nrow = h, ncol = nmethods, byrow = TRUE)
        wConvex$W <- 0.5*(foo2/rowSums(foo2)+foo4/rowSums(foo4))
      }
      else #SMAPE
      {
        wConvex$Ws <- matrix(foo1/sum(foo1), nrow = h, ncol = nmethods, byrow = TRUE)
        wConvex$W <- foo2/rowSums(foo2)
      }
      return(wConvex)
    },
    error = function(e)
    {
      return(list(W=NA))
    }
  )
  return(out)
}

#Training phase
#####################################################################################
#Number of training series per cluster
trainCount <- ceiling(clusterCount*pTrain/100)

#Total training series
maxtrain <- sum(trainCount[inicluster:fincluster]) 

nmethods = 5 #Number of methods (pool of methods)
h <- Mseries[[1]]$h #Forecast horizon 
#Individual weights per training series - Horizon-optimized (W) or Static (Ws)
#error = {0 = no error, 1 = error}
#small = {0 = not small, 1 = small series}
wTrain <- list(seriesId = vector("character", maxtrain),
               seriesLen = vector("integer", maxtrain),
               W = array(dim = c(h, nmethods, maxtrain)),
               Ws = array(dim = c(h, nmethods, maxtrain)),
               error = vector("integer", maxtrain),
               small = vector("integer", maxtrain))
nseries <- 0
nsmall <- 0
nerrors <- 0
ptm <- proc.time() #Timer

#Cluster loop
for (iCluster in inicluster:fincluster)
{
  ntrain <- trainCount[iCluster]

  #Not empty cluster?
  if (ntrain > 0) {
    index <- which(clusterList$clusterId == iCluster)[1:ntrain]
    Mtrain <- Mseries[index]
    iniseries <- 1
    
    # ######Use this block only if retraining a cluster after error
    # if (iCluster == 18) {
    #   nseries <- 2276
    #   iniseries <- 2277
    # }
  
    #Series loop
    for (iseries in iniseries:length(Mtrain))
    {
      #Series
      id <- Mtrain[[iseries]]$st        #Name of the series
      xtx <- Mtrain[[iseries]]$x        #The whole historical series (time series of length n)
      seriesLen <- Mtrain[[iseries]]$n  #Time series length
      
      nseries <- nseries + 1
      cat("Time Series: ", id, iCluster, "/", nseries, "/", maxtrain, "/", seriesLen, "\n")
      Weq <- matrix(1/nmethods, nrow=h, ncol=nmethods) #Equal weights matrix

      error <- 0
      #Weights calculation
      if (seriesLen >= (2*h+minorigins-1)) #Big enough for at least minorigins?
      {
        foo <- wConvex5(xtx,h,maxorigins,mytype)
        W <- foo$W
        Ws <- foo$Ws
        if (sum(is.na(W))>0)
        {
          cat("Error!\n")
          nerrors <- nerrors + 1
          wTrain$error[nseries] <- 1
          W <- Weq #Weights = Equal weights
          Ws <- Weq 
        }
      }
      else
      {
        cat("Small!\n")
        nsmall <- nsmall + 1
        wTrain$small[nseries] <- 1
        W <- Weq #Weights = Equal weights
        Ws <- Weq 
      }

      #Individual weights per training series
      wTrain$seriesId[nseries] <- id
      wTrain$seriesLen[nseries] <- seriesLen
      wTrain$W[,,nseries] <- W
      wTrain$Ws[,,nseries] <- Ws
 
      #Saving results so far
      elapsed <- proc.time() - ptm
      timeTrain = unname(elapsed[3]/3600)
      save(iCluster, iseries, nseries, timeTrain, nerrors, nsmall, trainCount, wTrain, file = fileTrain)
      
    }#End series loop

  }#End If

}#End cluster loop

cat("\nElapsed time - TRAINING (h):", timeTrain, "\n")
cat("\nCalc. errors:", nerrors, "\n")
cat("\nSmall series:", nsmall, "\n")