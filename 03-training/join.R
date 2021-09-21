#Join trained clusters, take clusters means in the proccess

#==== Output file
fileTrain <- "trainLog.RData"

#==== Initialization...
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load("trainLog01.RData")
h <- dim(wTrain$W)[1]
nmethods <- dim(wTrain$W)[2]
maxtrain <- sum(trainCount)
nclusters <- length(trainCount)

#Individual weights per training series - Horizon-optimized (W) or Static (Ws)
#error = {0 = no error, 1 = error}
#small = {0 = not small, 1 = small series}
wTrain2 <- list(seriesId = vector("character", maxtrain),
                seriesLen = vector("integer", maxtrain),
                W = array(dim = c(h, nmethods, maxtrain)),
                Ws = array(dim = c(h, nmethods, maxtrain)),
                error = vector("integer", maxtrain),
                small = vector("integer", maxtrain))

nsmall2 <- vector("numeric", nclusters)
nerrors2 <- vector("numeric", nclusters)
timeTrain2 <- vector("numeric", nclusters)

#Mean weights per cluster - horizon-optimized
wCluster <- array(0, dim = c(h,nmethods,nclusters))

#Mean weights per cluster - static
wClusterST <- array(0, dim = c(h,nmethods,nclusters))

#==== Joining clusters...
indseries <- 1

cat("\nCluster...\n")
#====Cluster 1
print(thisCluster <- 1)
nseriesCluster <- trainCount[thisCluster]
print(nseriesCluster)
load("trainLog01.RData")
foo <- which(wTrain$seriesId=="")[1]
if (is.na(foo)) {
  fin <- length(wTrain$seriesId) 
} else {
  fin <- foo-1
}
print(fin)
for (i in 1:fin) {
  wTrain2$seriesId[indseries] <- wTrain$seriesId[i]
  wTrain2$seriesLen[indseries] <- wTrain$seriesLen[i]
  wTrain2$W[,,indseries] <- wTrain$W[,,i]
  wTrain2$Ws[,,indseries]<- wTrain$Ws[,,i]
  wTrain2$error[indseries] <- wTrain$error[i]
  wTrain2$small[indseries] <- wTrain$small[i]
  indseries <- indseries+1
  #For the means calculation:
  wCluster[,,thisCluster] <- wCluster[,,thisCluster] + wTrain$W[,,i]
  wClusterST[,,thisCluster] <- wClusterST[,,thisCluster] + wTrain$Ws[,,i]
}
nsmall2[thisCluster] <- nsmall
nerrors2[thisCluster] <- nerrors
timeTrain2[thisCluster] <- timeTrain
#Cluster means:
wCluster[,,thisCluster] <- wCluster[,,thisCluster]/nseriesCluster
wClusterST[,,thisCluster] <- wClusterST[,,thisCluster]/nseriesCluster

cat("\nCluster...\n")
#====Cluster 2
print(thisCluster <- 2)
nseriesCluster <- trainCount[thisCluster]
print(nseriesCluster)
load("trainLog02.RData")
foo <- which(wTrain$seriesId=="")[1]
if (is.na(foo)) {
  fin <- length(wTrain$seriesId) 
} else {
  fin <- foo-1
}
print(fin)
for (i in 1:fin) {
  wTrain2$seriesId[indseries] <- wTrain$seriesId[i]
  wTrain2$seriesLen[indseries] <- wTrain$seriesLen[i]
  wTrain2$W[,,indseries] <- wTrain$W[,,i]
  wTrain2$Ws[,,indseries]<- wTrain$Ws[,,i]
  wTrain2$error[indseries] <- wTrain$error[i]
  wTrain2$small[indseries] <- wTrain$small[i]
  indseries <- indseries+1
  #For the means calculation:
  wCluster[,,thisCluster] <- wCluster[,,thisCluster] + wTrain$W[,,i]
  wClusterST[,,thisCluster] <- wClusterST[,,thisCluster] + wTrain$Ws[,,i]
}
nsmall2[thisCluster] <- nsmall
nerrors2[thisCluster] <- nerrors
timeTrain2[thisCluster] <- timeTrain
#Cluster means:
wCluster[,,thisCluster] <- wCluster[,,thisCluster]/nseriesCluster
wClusterST[,,thisCluster] <- wClusterST[,,thisCluster]/nseriesCluster

cat("\nCluster...\n")
#====Cluster 3
print(thisCluster <- 3)
nseriesCluster <- trainCount[thisCluster]
print(nseriesCluster)
load("trainLog03.RData")
foo <- which(wTrain$seriesId=="")[1]
if (is.na(foo)) {
  fin <- length(wTrain$seriesId) 
} else {
  fin <- foo-1
}
print(fin)
for (i in 1:fin) {
  wTrain2$seriesId[indseries] <- wTrain$seriesId[i]
  wTrain2$seriesLen[indseries] <- wTrain$seriesLen[i]
  wTrain2$W[,,indseries] <- wTrain$W[,,i]
  wTrain2$Ws[,,indseries]<- wTrain$Ws[,,i]
  wTrain2$error[indseries] <- wTrain$error[i]
  wTrain2$small[indseries] <- wTrain$small[i]
  indseries <- indseries+1
  #For the means calculation:
  wCluster[,,thisCluster] <- wCluster[,,thisCluster] + wTrain$W[,,i]
  wClusterST[,,thisCluster] <- wClusterST[,,thisCluster] + wTrain$Ws[,,i]
}
nsmall2[thisCluster] <- nsmall
nerrors2[thisCluster] <- nerrors
timeTrain2[thisCluster] <- timeTrain
#Cluster means:
wCluster[,,thisCluster] <- wCluster[,,thisCluster]/nseriesCluster
wClusterST[,,thisCluster] <- wClusterST[,,thisCluster]/nseriesCluster

cat("\nCluster...\n")
#====Cluster 10
print(thisCluster <- 10)
nseriesCluster <- trainCount[thisCluster]
print(nseriesCluster)
load("trainLog10.RData")
foo <- which(wTrain$seriesId=="")[1]
if (is.na(foo)) {
  fin <- length(wTrain$seriesId) 
} else {
  fin <- foo-1
}
print(fin)
for (i in 1:fin) {
  wTrain2$seriesId[indseries] <- wTrain$seriesId[i]
  wTrain2$seriesLen[indseries] <- wTrain$seriesLen[i]
  wTrain2$W[,,indseries] <- wTrain$W[,,i]
  wTrain2$Ws[,,indseries]<- wTrain$Ws[,,i]
  wTrain2$error[indseries] <- wTrain$error[i]
  wTrain2$small[indseries] <- wTrain$small[i]
  indseries <- indseries+1
  #For the means calculation:
  wCluster[,,thisCluster] <- wCluster[,,thisCluster] + wTrain$W[,,i]
  wClusterST[,,thisCluster] <- wClusterST[,,thisCluster] + wTrain$Ws[,,i]
}
nsmall2[thisCluster] <- nsmall
nerrors2[thisCluster] <- nerrors
timeTrain2[thisCluster] <- timeTrain
#Cluster means:
wCluster[,,thisCluster] <- wCluster[,,thisCluster]/nseriesCluster
wClusterST[,,thisCluster] <- wClusterST[,,thisCluster]/nseriesCluster

cat("\nCluster...\n")
#====Cluster 11
print(thisCluster <- 11)
nseriesCluster <- trainCount[thisCluster]
print(nseriesCluster)
load("trainLog11.RData")
foo <- which(wTrain$seriesId=="")[1]
if (is.na(foo)) {
  fin <- length(wTrain$seriesId) 
} else {
  fin <- foo-1
}
print(fin)
for (i in 1:fin) {
  wTrain2$seriesId[indseries] <- wTrain$seriesId[i]
  wTrain2$seriesLen[indseries] <- wTrain$seriesLen[i]
  wTrain2$W[,,indseries] <- wTrain$W[,,i]
  wTrain2$Ws[,,indseries]<- wTrain$Ws[,,i]
  wTrain2$error[indseries] <- wTrain$error[i]
  wTrain2$small[indseries] <- wTrain$small[i]
  indseries <- indseries+1
  #For the means calculation:
  wCluster[,,thisCluster] <- wCluster[,,thisCluster] + wTrain$W[,,i]
  wClusterST[,,thisCluster] <- wClusterST[,,thisCluster] + wTrain$Ws[,,i]
}
nsmall2[thisCluster] <- nsmall
nerrors2[thisCluster] <- nerrors
timeTrain2[thisCluster] <- timeTrain
#Cluster means:
wCluster[,,thisCluster] <- wCluster[,,thisCluster]/nseriesCluster
wClusterST[,,thisCluster] <- wClusterST[,,thisCluster]/nseriesCluster

cat("\nCluster...\n")
#====Cluster 12
print(thisCluster <- 12)
nseriesCluster <- trainCount[thisCluster]
print(nseriesCluster)
load("trainLog12.RData")
foo <- which(wTrain$seriesId=="")[1]
if (is.na(foo)) {
  fin <- length(wTrain$seriesId) 
} else {
  fin <- foo-1
}
print(fin)
for (i in 1:fin) {
  wTrain2$seriesId[indseries] <- wTrain$seriesId[i]
  wTrain2$seriesLen[indseries] <- wTrain$seriesLen[i]
  wTrain2$W[,,indseries] <- wTrain$W[,,i]
  wTrain2$Ws[,,indseries]<- wTrain$Ws[,,i]
  wTrain2$error[indseries] <- wTrain$error[i]
  wTrain2$small[indseries] <- wTrain$small[i]
  indseries <- indseries+1
  #For the means calculation:
  wCluster[,,thisCluster] <- wCluster[,,thisCluster] + wTrain$W[,,i]
  wClusterST[,,thisCluster] <- wClusterST[,,thisCluster] + wTrain$Ws[,,i]
}
nsmall2[thisCluster] <- nsmall
nerrors2[thisCluster] <- nerrors
timeTrain2[thisCluster] <- timeTrain
#Cluster means:
wCluster[,,thisCluster] <- wCluster[,,thisCluster]/nseriesCluster
wClusterST[,,thisCluster] <- wClusterST[,,thisCluster]/nseriesCluster

#====Saving
wTrain <- wTrain2
nsmall <- nsmall2
nerrors <- nerrors2
timeTrain <- timeTrain2
save(trainCount, wTrain, nsmall, nerrors, timeTrain, wCluster, wClusterST, file = fileTrain)