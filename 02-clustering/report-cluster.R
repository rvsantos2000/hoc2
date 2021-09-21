#Plots

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load("clusterLog.RData")

#Plotting cluster frequencies
par(mai = c(0.5,1.5,0.1,0.5)) #Bottom, left, up, right
data <- data.frame(clusterCount,matrix(ets_lookup))
data <- data[c(18:10,6:1),]
#mylabel1 <- data[,1] #absolute labels
mylabel1 <- data[,1]/sum(data[,1]) #percentage labels
mylabel2 <- data[,2] 

foolabel <- rep(NA,15)
barcolor <- rep("light grey",15)
foomax <- max(mylabel1)
foomin <- min(mylabel1[mylabel1>0]) #Ignore 0s

foolabel[which(mylabel1==foomax)] = foomax
foolabel[which(mylabel1==foomin)] = foomin
barcolor[which(mylabel1==foomax)] = "cyan"

bp<-barplot(height = mylabel1, names.arg = mylabel2, las = 1, horiz = TRUE, col = barcolor)
foolabel <- paste(round(100*foolabel, 1), "%", sep="")
foolabel[foolabel == 'NA%'] <- NA
text(0,bp,foolabel,cex=1,pos=4, offset =0.7)

cat("\nElapsed time - CLUSTERING (h):", timeCluster, "\n")
cat(sprintf("#Time-series: %s", sum(clusterCount)))