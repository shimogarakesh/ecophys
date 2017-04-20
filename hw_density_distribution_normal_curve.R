
setwd("//Volumes/Extended/INMET/allstations/")

dfm <- read.csv("percentile_occ.csv", header = TRUE, sep = ",")
dfm$d3_tx90 <- NULL
dfm$d3_tn90 <- NULL

p1 <- rnorm(length(dfm$d1_tx90), mean(dfm$d1_tx90, na.rm = TRUE), sd(dfm$d1_tx90, na.rm = TRUE))
p2 <- rnorm(length(dfm$d2_tx90), mean(dfm$d2_tx90, na.rm = TRUE), sd(dfm$d2_tx90, na.rm = TRUE))
p3 <- rnorm(length(dfm$d1_tn90), mean(dfm$d1_tn90, na.rm = TRUE), sd(dfm$d1_tn90, na.rm = TRUE))
p4 <- rnorm(length(dfm$d2_tn90), mean(dfm$d2_tn90, na.rm = TRUE), sd(dfm$d2_tn90, na.rm = TRUE))



quartz()
png(filename = paste("rnorm_All_sumtx90p.png"), width = 500, height= 500, res = 90)
hist (p1, prob=FALSE, breaks=20, col=rgb(0.5,0,0,0.4), xlab="Days with Tx >90p", ylim=c(0,25), main=NULL, xlim=c(0,1000), ylab = "Stations")
hist (p2, prob=FALSE, breaks=10, col=rgb(0,0,0.5,0.4), add=TRUE)
leg_txt <- c("1997-2006","2007-2016")
col_code <- c(rgb(0,0,0.5,0.4), rgb(0.5,0,0,0.4))
legend("topleft", legend = leg_txt, fill=col_code, horiz = FALSE, cex = 1, border="black", box.col = NULL, bty = "n")
dev.off()
graphics.off()  

quartz()
png(filename = paste("rnorm_All_sumtn90p.png"), width = 500, height= 500, res = 90)
hist (p3, prob=FALSE, breaks=20, col=rgb(0.5,0,0,0.4), xlab="Days with Tn >90p", ylim=c(0,25), main=NULL, xlim=c(0,1000), ylab = "Stations")
hist (p4, prob=FALSE,  breaks=10, col=rgb(0,0,0.5,0.4), add=TRUE)
leg_txt <- c("1997-2006","2007-2016")
col_code <- c(rgb(0,0,0.5,0.4), rgb(0.5,0,0,0.4))
legend("topleft", legend = leg_txt, fill=col_code, horiz = FALSE, cex = 1, border="black", box.col = NULL, bty = "n")
dev.off()
graphics.off()  


CW <- subset(dfm, region == "CW") # D1 as 2007-2016 including both years - 10y
N <- subset(dfm, region == "N") # D1 as 2007-2016 including both years - 10y
NE <- subset(dfm, region == "NE") # D1 as 2007-2016 including both years - 10y
S <- subset(dfm, region == "S") # D1 as 2007-2016 including both years - 10y
SE <- subset(dfm, region == "SE") # D1 as 2007-2016 including both years - 10y


#--------------- Central western SKIPPED only 5 stations -----------------



#--------------- North east 5 stations -----------------

p1 <- rnorm(length(NE$d1_tx90), mean(NE$d1_tx90, na.rm = TRUE), sd(NE$d1_tx90, na.rm = TRUE))
p2 <- rnorm(length(NE$d2_tx90), mean(NE$d2_tx90, na.rm = TRUE), sd(NE$d2_tx90, na.rm = TRUE))
p3 <- rnorm(length(NE$d1_tn90), mean(NE$d1_tn90, na.rm = TRUE), sd(NE$d1_tn90, na.rm = TRUE))
p4 <- rnorm(length(NE$d2_tn90), mean(NE$d2_tn90, na.rm = TRUE), sd(NE$d2_tn90, na.rm = TRUE))




quartz()
png(filename = paste("rnorm_NE_sumtx90p.png"), width = 500, height= 500, res = 90)
hist (p1, prob=FALSE, breaks=10, col=rgb(0.5,0,0,0.4), xlab="Days with Tx >90p", ylim=c(0,8), main=NULL, xlim=c(0,1000), ylab = "Stations")
hist (p2, prob=FALSE, breaks=20, col=rgb(0,0,0.5,0.4), add=TRUE)
leg_txt <- c("1997-2006","2007-2016")
text(-1,7, "Northeast region", cex=0.9, col="grey20", adj = 0)
col_code <- c(rgb(0,0,0.5,0.4), rgb(0.5,0,0,0.4))
legend("topleft", legend = leg_txt, fill=col_code, horiz = FALSE, cex = 1, border="black", box.col = NULL, bty = "n")
dev.off()
graphics.off()  

quartz()
png(filename = paste("rnorm_NE_sumtn90p.png"), width = 500, height= 500, res = 90)
hist (p3, prob=FALSE, breaks=10, col=rgb(0.5,0,0,0.4), xlab="Days with Tn >90p", ylim=c(0,8), main=NULL, xlim=c(0,1000), ylab = "Stations")
hist (p4, prob=FALSE,  breaks=10, col=rgb(0,0,0.5,0.4), add=TRUE)
leg_txt <- c("1997-2006","2007-2016")
col_code <- c(rgb(0,0,0.5,0.4), rgb(0.5,0,0,0.4))
legend("topleft", legend = leg_txt, fill=col_code, horiz = FALSE, cex = 1, border="black", box.col = NULL, bty = "n")
text(-1,7, "Northeast region", cex=0.9, col="grey20", adj = 0)
dev.off()
graphics.off()  


#--------------- North -----------------

p1 <- rnorm(length(N$d1_tx90), mean(N$d1_tx90, na.rm = TRUE), sd(N$d1_tx90, na.rm = TRUE))
p2 <- rnorm(length(N$d2_tx90), mean(N$d2_tx90, na.rm = TRUE), sd(N$d2_tx90, na.rm = TRUE))
p3 <- rnorm(length(N$d1_tn90), mean(N$d1_tn90, na.rm = TRUE), sd(N$d1_tn90, na.rm = TRUE))
p4 <- rnorm(length(N$d2_tn90), mean(N$d2_tn90, na.rm = TRUE), sd(N$d2_tn90, na.rm = TRUE))




quartz()
png(filename = paste("rnorm_N_sumtx90p.png"), width = 500, height= 500, res = 90)
hist (p1, prob=FALSE, breaks=3, col=rgb(0.5,0,0,0.4), xlab="Days with Tx >90p", ylim=c(0,8), main=NULL, xlim=c(0,1000), ylab = "Stations")
hist (p2, prob=FALSE, breaks=3, col=rgb(0,0,0.5,0.4), add=TRUE)
leg_txt <- c("1997-2006","2007-2016")
text(-1,7, "Northeast region", cex=0.9, col="grey20", adj = 0)
col_code <- c(rgb(0,0,0.5,0.4), rgb(0.5,0,0,0.4))
legend("topleft", legend = leg_txt, fill=col_code, horiz = FALSE, cex = 1, border="black", box.col = NULL, bty = "n")
dev.off()
graphics.off()  

quartz()
png(filename = paste("rnorm_N_sumtn90p.png"), width = 500, height= 500, res = 90)
hist (p3, prob=FALSE, breaks=4, col=rgb(0.5,0,0,0.4), xlab="Days with Tn >90p", ylim=c(0,8), main=NULL, xlim=c(0,1000), ylab = "Stations")
hist (p4, prob=FALSE,  breaks=2, col=rgb(0,0,0.5,0.4), add=TRUE)
leg_txt <- c("1997-2006","2007-2016")
col_code <- c(rgb(0,0,0.5,0.4), rgb(0.5,0,0,0.4))
legend("topleft", legend = leg_txt, fill=col_code, horiz = FALSE, cex = 1, border="black", box.col = NULL, bty = "n")
text(-1,7, "Northeast region", cex=0.9, col="grey20", adj = 0)
dev.off()
graphics.off()  



