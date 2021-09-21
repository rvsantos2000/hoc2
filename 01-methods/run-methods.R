#####################################################################################
#HOC2 - M Competitions
#####################################################################################
library(forecast)

#Input data
#####################################################################################
setwd("D:/@Local/R Scripts/HOC2/M4")
load("M4Data.RData")
Mseries <- Filter(function(l) l$period == "Yearly", M4)

#Output files
#####################################################################################
setwd("D:/@Local/R Scripts/HOC2/M4/year/methods")
file0 <- "methods.csv"        
file2 <- "methods-metrics.csv"
file3 <- "methods-upper.csv"
file4 <- "methods-lower.csv"
fileMethods <- "methods.RData"

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

#Models predictions
#####################################################################################
allseries <- length(Mseries)
band <- 1:allseries

ptm <- proc.time() #Timer
#Series loop
for (iseries in band)
{
  #Series
  id <- Mseries[[iseries]]$st     #Name of the series
  h <- Mseries[[iseries]]$h       #The number of required forecasts    
  xtx <- Mseries[[iseries]]$x     #The whole historical series (time series of length n)
  xts <- Mseries[[iseries]]$xx    #The future data (time series of length h)
  
  cat("Time Series: ", id, '\n')
  
  #==== OUT-OF-SAMPLE TEST
  y1 <- forecast(ets(xtx), h = h, level = 95)
  y2 <- forecast(auto.arima(xtx), h = h, level = 95)
  y3 <- thetaf(xtx, h = h, level = 95) 
  y4 <- forecast(tbats(xtx), h = h, level = 95)
  y5 <- snaive(xtx, h = h, level = 95)
  models = cbind(y1$mean,y2$mean,y3$mean,y4$mean,y5$mean)
  models_up = cbind(y1$upper,y2$upper,y3$upper,y4$upper,y5$upper)
  models_lo = cbind(y1$lower,y2$lower,y3$lower,y4$lower,y5$mean)
  
  #==Models metrics
  #ETS
  smape1 <- mean(smape_cal(xts, y1$mean))
  mase1 <- mean(mase_cal(xtx, xts, y1$mean))
  msis1 <- msis_cal(xtx, xts, y1$upper, y1$lower, 0.05)
  ac1 <- ac_cal(xts, y1$upper, y1$lower)
  metrics1 = c(smape1, mase1, msis1, ac1)
  #AUTOARIMA
  smape2 <- mean(smape_cal(xts, y2$mean))
  mase2 <- mean(mase_cal(xtx, xts, y2$mean))
  msis2 <- msis_cal(xtx, xts, y2$upper, y2$lower, 0.05)
  ac2 <- ac_cal(xts, y2$upper, y2$lower)
  metrics2 = c(smape2, mase2, msis2, ac2)
  #THETA
  smape3 <- mean(smape_cal(xts, y3$mean))
  mase3 <- mean(mase_cal(xtx, xts, y3$mean))
  msis3 <- msis_cal(xtx, xts, y3$upper, y3$lower, 0.05)
  ac3 <- ac_cal(xts, y3$upper, y3$lower)
  metrics3 = c(smape3, mase3, msis3, ac3)
  #TBATS
  smape4 <- mean(smape_cal(xts, y4$mean))
  mase4 <- mean(mase_cal(xtx, xts, y4$mean))
  msis4 <- msis_cal(xtx, xts, y4$upper, y4$lower, 0.05)
  ac4 <- ac_cal(xts, y4$upper, y4$lower)
  metrics4 = c(smape4, mase4, msis4, ac4)
  #SNAIVE
  smape5 <- mean(smape_cal(xts, y5$mean))
  mase5 <- mean(mase_cal(xtx, xts, y5$mean))
  msis5 <- msis_cal(xtx, xts, y5$upper, y5$lower, 0.05)
  ac5 <- ac_cal(xts, y5$upper, y5$lower)
  metrics5 = c(smape5, mase5, msis5, ac5)
  
  #Equal weights combination
  Weq <- matrix(1/5, nrow=h, ncol=5) #Equal weights matrix
  yc <- rowSums(cbind(y1$mean,y2$mean,y3$mean,y4$mean,y5$mean)*Weq)
  #Prediction intervals (=ETS deltas)
  pd1 <- (y1$upper - y1$mean)  #ETS Upper bound delta
  pd2 <- (y1$mean - y1$lower)  #ETS Lower bound delta
  uc = yc + pd1   #HOC2 Upper bounds
  lc = yc - pd2   #HOC2 Lower bounds
  
  smape6 <- mean(smape_cal(xts, yc))
  mase6 <- mean(mase_cal(xtx, xts, yc))
  msis6 <- msis_cal(xtx, xts, uc, lc, 0.05)
  ac6 <- ac_cal(xts, uc, lc)
  metrics6 = c(smape6, mase6, msis6, ac6)
  
  #Binding
  models_metrics <- cbind(metrics1, metrics2, metrics3, metrics4, metrics5, metrics6)
  
  #Saving results
  write.table(data.frame(id,t(models)), file = file0, sep = ',', row.names = FALSE, col.names = FALSE, append = TRUE)
  write.table(data.frame(id,t(models_metrics)), file = file2, sep = ',', row.names = FALSE, col.names = FALSE, append = TRUE)
  write.table(data.frame(id,t(models_up)), file = file3, sep = ',', row.names = FALSE, col.names = FALSE, append = TRUE)
  write.table(data.frame(id,t(models_lo)), file = file4, sep = ',', row.names = FALSE, col.names = FALSE, append = TRUE)
  
  #Saving statistics
  elapsed <- proc.time() - ptm
  timeMethods = unname(elapsed[3]/3600)
  save(timeMethods, file = fileMethods)
  
  # #=========================== Plotting...
  # #Plotting - methods (out-of-sample)
  # P <- cbind(y1$mean, y2$mean, y3$mean, y4$mean, y5$mean, yc, xts)
  # plot(P[,7], ylim = c(min(P),max(P)), main = sprintf("Series %s", id), ylab = "Methods + Comb", lty = "dashed")
  # lines(P[,6], col = 'black')
  # lines(P[,1], col = 'blue')
  # lines(P[,2], col = 'red')
  # lines(P[,3], col = 'green')
  # lines(P[,4], col = 'gray')
  # lines(P[,5], col = 'magenta')
  # #=========================== End Plotting
  
} #End series loop
cat("\nElapsed time - METHODS (h):", timeMethods, "\n")