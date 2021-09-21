#Methods accuracy metrics
library(stringr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

fileMetrics <- "methods-metrics.csv"
fileLog <- "methods.RData"

nmethods <- 5 + 1 #Five + simple avg

m0 <- read.table(file = fileMetrics,sep=",", dec =".", col.names = c("series","smape", "mase", "msis", "ac"))
nlines <- dim(m0)[1]
mask <- vector(mode = "logical", length = nlines)
mask1 <- mask
mask2 <- mask
mask3 <- mask
mask4 <- mask
mask5 <- mask
mask6 <- mask
mask1[seq(1,nlines,nmethods)] <- TRUE
mask2[seq(2,nlines,nmethods)] <- TRUE
mask3[seq(3,nlines,nmethods)] <- TRUE
mask4[seq(4,nlines,nmethods)] <- TRUE
mask5[seq(5,nlines,nmethods)] <- TRUE
mask6[seq(6,nlines,nmethods)] <- TRUE

cat("\n==== ETS")
m <- m0[mask1,]
cat("\nSMAPE:", mean(m$smape))
cat("\nMASE:", mean(m$mase))
cat("\nMSIS:", mean(m$msis))
cat("\nAC:", mean(m$ac))
cat("\n")

cat("\n==== AUTOARIMA")
m <- m0[mask2,]
cat("\nSMAPE:", mean(m$smape))
cat("\nMASE:", mean(m$mase))
cat("\nMSIS:", mean(m$msis))
cat("\nAC:", mean(m$ac))
cat("\n")

cat("\n==== THETA")
m <- m0[mask3,]
cat("\nSMAPE:", mean(m$smape))
cat("\nMASE:", mean(m$mase))
cat("\nMSIS:", mean(m$msis))
cat("\nAC:", mean(m$ac))
cat("\n")

cat("\n==== TBATS")
m <- m0[mask4,]
cat("\nSMAPE:", mean(m$smape))
cat("\nMASE:", mean(m$mase))
cat("\nMSIS:", mean(m$msis))
cat("\nAC:", mean(m$ac))
cat("\n")

cat("\n==== SNAIVE")
m <- m0[mask5,]
cat("\nSMAPE:", mean(m$smape))
cat("\nMASE:", mean(m$mase))
cat("\nMSIS:", mean(m$msis))
cat("\nAC:", mean(m$ac))
cat("\n")

cat("\n==== Simple AVG")
m <- m0[mask6,]
cat("\nSMAPE:", mean(m$smape))
cat("\nMASE:", mean(m$mase))
cat("\nMSIS:", mean(m$msis))
cat("\nAC:", mean(m$ac))
cat("\n")

load(file = fileLog)
cat("\nElapsed time - MODELS (h):", timeMethods, "\n")