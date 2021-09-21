#####################################################################################
#HOC2 - M Competitions
#####################################################################################
library(forecast)

#Input data
#####################################################################################
setwd("D:/@Local/R Scripts/HOC2/M4")
load("M4Data.RData")
Mseries <- Filter(function(l) l$period == "Yearly", M4)
rm(M4)
h <- Mseries[[1]]$h

setwd("D:/@Local/R Scripts/HOC2/M4/year/methods")
fileMo01 <- "methods.csv"
fileLo01 <- "methods-lower.csv"
fileUp01 <- "methods-upper.csv"
m01 <- read.table(file=fileMo01,sep=",", dec =".", col.names = c("series", paste0("h", seq(h))))
l01 <- read.table(file=fileLo01,sep=",", dec =".", col.names = c("series", paste0("h", seq(h))))
u01 <- read.table(file=fileUp01,sep=",", dec =".", col.names = c("series", paste0("h", seq(h))))
tabModel <- rbind(m01)[2:(h+1)] #(m01, m02, m03...)
tabLower <- rbind(l01)[2:(h+1)] #(l01, l02, l03...)
tabUpper <- rbind(u01)[2:(h+1)] #(u01, u02, u03...)
rm(m01, l01, u01) #(m02, l02, u02...)
nmethods <- 5 
nlines <- dim(tabModel)[1]
mask <- mask1 <- mask2 <- mask3 <- mask4 <- mask5 <- vector(mode = "logical", length = nlines)
mask1[seq(1,nlines,nmethods)] <- TRUE
mask2[seq(2,nlines,nmethods)] <- TRUE
mask3[seq(3,nlines,nmethods)] <- TRUE
mask4[seq(4,nlines,nmethods)] <- TRUE
mask5[seq(5,nlines,nmethods)] <- TRUE
etsModel <- tabModel[mask1,]; etsLower <- tabLower[mask1,]; etsUpper <- tabUpper[mask1,]
arimaModel <- tabModel[mask2,]
thetaModel <- tabModel[mask3,]
tbatsModel <- tabModel[mask4,]
snaiveModel <- tabModel[mask5,]
rm(mask, mask1, mask2, mask3, mask4, mask5)

setwd("D:/@Local/R Scripts/HOC2/M4/year/clustering")
load("clusterLog.RData")
#clusterCount = number of series per cluster;
#clusterList = list with as many elements as the total number of series:
#clusterList$seriesId = series ID;
#clusterList$clusterId = cluster ID, 1:18 (7, 8 and 9 always empty).

setwd("D:/@Local/R Scripts/HOC2/M4/year/training")
load("trainLog.RData")
#wTrain list with as many elements as the total number of TRAINING series:
#wTrain$seriesId = training series ID;
#wTrain$seriesLen = training series length;
#wTrain$W = Series-specific horizon-optimized weights;
#wTrain$Ws = Series-specific static weights;
#trainCount = Number of training series per cluster;
#wCluster[h,nmethods,cluster] = Mean weights per cluster - horizon-optimized;
#wClusterST[h,nmethods,cluster] = Mean weights per cluster - static.
#trainCount <- ceiling(clusterCount*pTrain/100)

#Output files
#####################################################################################
setwd("D:/@Local/R Scripts/HOC2/M4/year/test")
fileTest <- "testLog.RData"
file1 <- "weights.csv"
file5 <- "hoc2.csv"
file6 <- "hoc2-upper.csv"
file7 <- "hoc2-lower.csv"
file8 <- "hoc2-metrics.csv"

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
# PI evaluation functions (by Erick)----
msis_cal <- function(insample, outsample, upper, lower, a){
  #Used to estimate MSIS
  frq <- frequency(insample)
  forecastsNaiveSD <- rep(NA,frq)
  for (j in (frq+1):length(insample)){
    forecastsNaiveSD <- c(forecastsNaiveSD, insample[j-frq])
  }
  masep<-mean(abs(insample-forecastsNaiveSD),na.rm = TRUE)
  
  width <- sum(upper-lower)
  
  belowdiffs <- lower-outsample
  below <- sum(belowdiffs[belowdiffs>0])
  
  abovediffs <- outsample-upper
  above <- sum(abovediffs[abovediffs>0])
  
  msis <- (width + 2/a*(below+above))/(masep*length(outsample))
  
  return(msis)
}
ac_cal <- function(outsample, upper, lower){
  #Used to estimate Average Coverage (AC)
  belowdiffs <- lower-outsample
  below <- length(which(belowdiffs>0))
  
  abovediffs <- outsample-upper
  above <- length(which(abovediffs>0))
  
  counts <- below + above
  
  notcovered <- counts/length(outsample)
  ac <- 1-notcovered
  return(ac)
}
#To get Absolute Coverage Difference (ACD, Makridakis/2020):
#abs(ac_cal(outsample, upper, lower) - 0.95)

#Test phase
#####################################################################################
#Runs through the full set of series, making out-of-sample predictions for all methods;
#For each series, decides which weighting matrix to use in the following way: 
#
#1) If the series is big enough and was in the training set:
#   W4) Series-specific horizon-optimized weights.
#
#2) If the series is small* OR was not in the training set:
#   W1) Horizon-optimized weights for the series cluster.
#
#*If the series is too small (< 2*h), applies equal weights.
#####################################################################################
thisCount <- vector("integer", length(clusterCount))
minorigins <- 5 #Minimum number of rolling origins
nmethods = dim(wTrain$W[,,1])[2] #Number of methods (pool of methods)
allseries <- length(Mseries)
band <- 1:allseries

nerrors <- 0
nsmall <- 0
n2small <- 0
nequals <- 0
nw1 <- 0
nw2 <- 0
nw3 <- 0
nw4 <- 0
nw5 <- 0
vecSel <- vector("integer", length(band)) #Indicates selected weights per series {1=W1,2=W2,3=W3,4=W4,5=W5}
vecError <- vector("integer", length(band)) #error = {0 = no error, 1 = error}
vecSmall <- vector("integer", length(band)) #error = {0 = not small, 1 = small}
vec2Small <- vector("integer", length(band)) #error = {0 = not small, 1 = small}
ptm <- proc.time() #Timer
#Series loop
for (iseries in band)
{
  #Series
  id <- Mseries[[iseries]]$st        #Name of the series
  h <- Mseries[[iseries]]$h          #The number of required forecasts    
  xtx <- Mseries[[iseries]]$x        #The whole historical series (time series of length n)
  xts <- Mseries[[iseries]]$xx       #The future data (time series of length h)
  seriesLen <- Mseries[[iseries]]$n  #Time series length
  
  thisCluster <- clusterList$clusterId[iseries]
  thisCount[thisCluster] <- thisCount[thisCluster] + 1
  cat("Time Series: ", id, thisCluster, "/", seriesLen, "\n")

  #Basic weighting matrices
  W1 <- wCluster[,,thisCluster] #Horizon-optimized weights for the series cluster 
  W2 <- wClusterST[,,thisCluster] #Static mean weights for the series cluster
  W3 <- matrix(1/nmethods, nrow=h, ncol=nmethods) #Equal weights
  
  #==== IN-SAMPLE WEIGHTS SELECTION
  if (seriesLen >= (2*h+minorigins-1)) #Big enough for at least minorigins?
  {
    vecSmall[iseries] <- 0
    isTraining <- (thisCount[thisCluster] <= trainCount[thisCluster])
    if (isTraining) #The series WAS in the training set
    { 
      W4 <- wTrain$W[,,which(wTrain$seriesId == id)]
      W <- W4
      vecSel[iseries] <- 4
      vecError[iseries] <- 0 
    }
    else #The series WAS NOT in the training set
    {
      W <- W1
      vecSel[iseries] <- 1
      vecError[iseries] <- 0 
    }
  }
  else #Series is SMALL (and maybe too small)
  {
    cat("Small!\n")
    nsmall <- nsmall + 1
    vecSmall[iseries] <- 1
    if (seriesLen < (2*h)) #TOO SMALL?
    { 
      cat("Too small!\n")
      n2small <- n2small + 1
      vec2Small[iseries] <- 1
      W <- W3 #Equal weights
      vecSel[iseries] <- 3
      vecError[iseries] <- 0
    }
    else
    {
      W <- W1
      vecSel[iseries] <- 1
      vecError[iseries] <- 0 
    }
  }

  #==== SELECTION REPORT
  switch(vecSel[iseries], 
         "1" = {nw1 <- nw1+1; cat("W1!\n")},
         "2" = {nw2 <- nw2+1; cat("W2!\n")},
         "3" = {nw3 <- nw3+1; cat("W3!\n")},
         "4" = {nw4 <- nw4+1; cat("W4!\n")},
         "5" = {nw5 <- nw5+1; cat("W5!\n")})
  #Is W a matrix of only equal weights?
  diffe <- (W == 1/nmethods)
  if (sum(rowSums(diffe)==nmethods) == h)
  {
    cat("Equal!\n")
    nequals <- nequals + 1
  }
  
  #==HOC2 COMBINATION, with previously selected weights
  #Point forecasts
  y1 = etsModel[iseries,]
  y2 = arimaModel[iseries,]
  y3 = thetaModel[iseries,]
  y4 = tbatsModel[iseries,]
  y5 = snaiveModel[iseries,]
  yc <- rowSums(t(rbind(y1,y2,y3,y4,y5))*W)
  #Prediction intervals (=ETS deltas)
  pd1 <- t(etsUpper[iseries,] - etsModel[iseries,])  #ETS Upper bound delta
  pd2 <- t(etsModel[iseries,] - etsLower[iseries,])  #ETS Lower bound delta
  uc = yc + pd1   #HOC2 Upper bounds
  lc = yc - pd2   #HOC2 Lower bounds

  #==HOC2 metrics
  smape <- mean(smape_cal(xts, yc))
  mase <- mean(mase_cal(xtx, xts, yc))
  msis <- msis_cal(xtx, xts, uc, lc, 0.05)
  ac <- ac_cal(xts, uc, lc)
  hoc2_metrics = c(smape, mase, msis, ac)
  
  #Saving results
  write.table(data.frame(id,t(W)), file = file1, sep = ',', row.names = FALSE, col.names = FALSE, append = TRUE)
  write.table(data.frame(id,t(yc)), file = file5, sep = ',', row.names = FALSE, col.names = FALSE, append = TRUE)
  write.table(data.frame(id,t(uc)), file = file6, sep = ',', row.names = FALSE, col.names = FALSE, append = TRUE)
  write.table(data.frame(id,t(lc)), file = file7, sep = ',', row.names = FALSE, col.names = FALSE, append = TRUE)
  write.table(data.frame(id,t(hoc2_metrics)), file = file8, sep = ',', row.names = FALSE, col.names = FALSE, append = TRUE)
  
  #Saving statistics
  elapsed <- proc.time() - ptm
  timeTest = unname(elapsed[3]/3600)
  save(timeTest, nw1, nw2, nw3, nw4, nw5, nsmall, n2small, nequals, nerrors, vecSmall, vec2Small, vecError, vecSel, file = fileTest)
  
  # #=========================== Plotting...
  # #Plotting - methods + convex combination (out-of-sample)
  # P <- cbind(t(y1), t(y2), t(y3), t(y4), t(y5), yc, xts)
  # plot(P[,7], ylim = c(min(P),max(P)), main = sprintf("Series %s", id), ylab = "Methods + Comb", lty = "dashed")
  # lines(P[,6], col = 'black')
  # lines(P[,1], col = 'blue')
  # lines(P[,2], col = 'red')
  # lines(P[,3], col = 'green')
  # lines(P[,4], col = 'gray')
  # lines(P[,5], col = 'magenta')
  # 
  # #Plotting - weights (out-of-sample))
  # plot(W[,1], ylim = c(min(W),max(W)), type='l', col = 'blue', main = sprintf("Series %s", id), ylab = "Weights")
  # lines(W[,2], col = 'red')
  # lines(W[,3], col = 'green')
  # lines(W[,4], col = 'gray')
  # lines(W[,5], col = 'magenta')
  # #=========================== End Plotting
  
} #End series loop
cat("\nElapsed time - TEST (h):", timeTest, "\n")
cat("\nW1:", nw1, "\n")
cat("\nW2:", nw2, "\n")
cat("\nW3:", nw3, "\n")
cat("\nW4:", nw4, "\n")
cat("\nW5:", nw5, "\n")
cat("\nCalc. errors:", nerrors, "\n")
cat("\nSmall series:", nsmall, "\n")
cat("\nToo small series:", n2small, "\n")
cat("\nEqual weights:", nequals, "\n")