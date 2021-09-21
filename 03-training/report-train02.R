#Plots
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load("trainLog.RData")

#indcluster <- c(1:6,10:18)
indcluster <- 11

mycolor = c("blue", "red", "black", "orange", "magenta")
mylegend = c("ets", "arima", "theta", "tbats", "snaive")
myltype = c("solid", "dashed", "dotted", "dotdash", "twodash")
mylwd = 2
#par(mai = c(1,1,0.2,1)) #Bottom, left, up, right
par(mai = c(1,1,1,1)) #Bottom, left, up, right
mylim = c(0.05,0.25)
hoc2 = 1

#Plotting mean weights per cluster - Horizon Optimized
for (i in indcluster) {
  mytitle = sprintf("Cluster %i",i)
  if (hoc2) {
    #Horizon-optimized
    W <- wCluster[,,i]
    plot(W[,1], ylim = mylim, type="l", lty=myltype[1], lwd = mylwd, col = mycolor[1], ylab = "weights", xlab = "horizon")
    for (j in 2:dim(W)[2]) {
      lines(W[,j], type = "l", col = mycolor[j], lty=myltype[j], lwd = mylwd)
    }
    legend('bottomright',legend = mylegend, col = mycolor, lty = myltype, lwd = mylwd)
    title(mytitle)
  } else {
    #Static
    W <- wClusterST[,,i]
    plot(W[,1], ylim = mylim, type="l", lty=myltype[1], lwd = mylwd, col = mycolor[1], ylab = "weights", xlab = "horizon")
    for (j in 2:dim(W)[2]) {
      lines(W[,j], type = "l", col = mycolor[j], lty=myltype[j], lwd = mylwd)
    }
    legend('bottomright',legend = mylegend, col = mycolor, lty = myltype, lwd = mylwd)
    title(mytitle)
  }
}