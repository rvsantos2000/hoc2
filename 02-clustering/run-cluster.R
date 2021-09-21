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
fileOut = "clusterLog.RData"

#####################################################################################
#ETS Lookup Table -----
ets_lookup <- list("ETS(A,N,N)", "ETS(A,A,N)", "ETS(A,Ad,N)",
                   "ETS(A,N,A)", "ETS(A,A,A)", "ETS(A,Ad,A)",
                   "ETS(A,N,M)", "ETS(A,A,M)", "ETS(A,Ad,M)",
                   "ETS(M,N,N)", "ETS(M,A,N)", "ETS(M,Ad,N)",
                   "ETS(M,N,A)", "ETS(M,A,A)", "ETS(M,Ad,A)",
                   "ETS(M,N,M)", "ETS(M,A,M)", "ETS(M,Ad,M)")

#####################################################################################
#=========================== Running...
nseries <- length(Mseries)
band <- 1:nseries

#List with as many elements as the total number of series;
#Indicates cluster id per series:
clusterList <- list(seriesId = vector("character", length(band)), 
                clusterId = vector("integer", length(band)))

#To count the number of series per cluster
clusterCount <- vector(mode = "integer", length = length(ets_lookup))

#Turn on timer
ptm <- proc.time()
for (iseries in band)
{
  #Series
  id <- Mseries[[iseries]]$st     #Name of the series
  h <- Mseries[[iseries]]$h       #The number of required forecasts    
  xtx <- Mseries[[iseries]]$x     #The whole historical series (time series of length n)

  cat("Time Series: ", id, '\n')
  
  #Fitting the whole historical set
  fit1 <- ets(xtx)
  ind <- which(ets_lookup ==  fit1$method)
  
  clusterList$seriesId[iseries] <- id
  clusterList$clusterId[iseries] <- ind 
  clusterCount[ind] <- clusterCount[ind]+1
  
  cat("Method:", fit1$method, '\n')
  cat("Lookup:", ind, "\n\n")

  elapsed <- proc.time() - ptm
}
timeCluster = unname(elapsed[3]/3600)
cat("\nElapsed time (h):", timeCluster, "\n")
save(ets_lookup, clusterList, clusterCount, timeCluster, file = fileOut)

#Plotting cluster frequencies...
clusterCount
par(mai = c(0.5,1.5,0.1,0.5)) #Bottom, left, up, right
data <- data.frame(clusterCount,matrix(ets_lookup))
bp<-barplot(height = data[c(18:10,6:1),1], names.arg = data[c(18:10,6:1),2], las = 1, horiz = TRUE)
text(0,bp,data[c(18:10,6:1),1],cex=1,pos=4)