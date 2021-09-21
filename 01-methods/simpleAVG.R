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

#Auxiliary functions
################################################################################
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

#Series Loop
###############################################################################
allseries <- length(Mseries)
band <- 1:allseries
metrics6 <- matrix(nrow = allseries, ncol = 4)
for (iseries in band)
{
  #Series
  id <- Mseries[[iseries]]$st        #Name of the series
  h <- Mseries[[iseries]]$h          #The number of required forecasts    
  xtx <- Mseries[[iseries]]$x        #The whole historical series (time series of length n)
  xts <- Mseries[[iseries]]$xx       #The future data (time series of length h)
  seriesLen <- Mseries[[iseries]]$n  #Time series length
  
  cat("Time Series: ", id, " / ", seriesLen, "\n")
  
  #Equal weights combination
  Weq <- matrix(1/nmethods, nrow=h, ncol=nmethods) #Equal weights matrix
  yc <- rowSums(t(rbind(etsModel[iseries,],arimaModel[iseries,],thetaModel[iseries,],tbatsModel[iseries,],snaiveModel[iseries,]))*Weq)
  #Prediction intervals (=ETS deltas)
  pd1 <- t(etsUpper[iseries,] - etsModel[iseries,])  #ETS Upper bound delta
  pd2 <- t(etsModel[iseries,] - etsLower[iseries,])  #ETS Lower bound delta
  uc = yc + pd1   #AVG Upper bounds
  lc = yc - pd2   #AVG Lower bounds
  
  smape6 <- mean(smape_cal(xts, yc))
  mase6 <- mean(mase_cal(xtx, xts, yc))
  msis6 <- msis_cal(xtx, xts, uc, lc, 0.05)
  ac6 <- ac_cal(xts, uc, lc)
  metrics6[iseries,] = c(smape6, mase6, msis6, ac6)
} 
metricsMeans <- colMeans(metrics6)
save (metricsMeans, file = "simpleAVG.RData")
cat("\n==== Simple AVG")
cat("\nSMAPE:", metricsMeans[1])
cat("\nMASE:", metricsMeans[2])
cat("\nMSIS:", metricsMeans[3])
cat("\n")