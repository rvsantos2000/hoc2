#Metrics
library(stringr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

fileMetrics <- "hoc2-metrics.csv"
fileLog <- "testLog.RData"

m <- read.table(file = fileMetrics,sep=",", dec =".", col.names = c("series","smape", "mase", "msis", "ac"))
cat("\nSMAPE:", mean(m$smape))
cat("\nMASE:", mean(m$mase))
cat("\nMSIS:", mean(m$msis))
cat("\nAC:", mean(m$ac))
cat("\n")

load(file = fileLog)
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