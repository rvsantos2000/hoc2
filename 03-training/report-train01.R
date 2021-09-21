#Plots
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load("trainLog.RData")

#ETS Lookup Table -----
ets_lookup <- list("ETS(A,N,N)", "ETS(A,A,N)", "ETS(A,Ad,N)",
                   "ETS(A,N,A)", "ETS(A,A,A)", "ETS(A,Ad,A)",
                   "ETS(A,N,M)", "ETS(A,A,M)", "ETS(A,Ad,M)",
                   "ETS(M,N,N)", "ETS(M,A,N)", "ETS(M,Ad,N)",
                   "ETS(M,N,A)", "ETS(M,A,A)", "ETS(M,Ad,A)",
                   "ETS(M,N,M)", "ETS(M,A,M)", "ETS(M,Ad,M)")

maxcluster <- which(trainCount == max(trainCount))
pcluster <- trainCount/sum(trainCount)*100

cat("\nTraining series:", sum(trainCount), "\n")
cat("\nTraining times (h):", round(timeTrain,2), "\n")
cat("\nCalc. errors:", nerrors, "\n")
cat("\nSmall series:", nsmall, "\n")
cat("\nCluster percentuals: ", round(pcluster,2), "\n")
cat("\nLargest cluster: ", maxcluster, " - ", ets_lookup[[maxcluster]], "\n")
cat("\nLargest cluster %: ", round(pcluster[maxcluster],2), "\n")
cat("\nLargest cluster time (h): ", round(timeTrain[maxcluster],2), "\n")
cat("\nEstimated total time (h): ", round(timeTrain[maxcluster]*100/pcluster[maxcluster],2), "\n")




