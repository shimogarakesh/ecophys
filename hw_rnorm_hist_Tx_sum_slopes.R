
setwd("//Volumes/Extended/INMET/allstations/")

dfm <- read.csv("slopes.csv", header = TRUE, sep = ",")

p1 <- rnorm(length(dfm$d1_txs_slope), mean(dfm$d1_txs_slope, na.rm = TRUE), sd(dfm$d1_txs_slope, na.rm = TRUE))
p2 <- rnorm(length(dfm$d2_txs_slope), mean(dfm$d2_txs_slope, na.rm = TRUE), sd(dfm$d2_txs_slope, na.rm = TRUE))
p3 <- rnorm(length(dfm$d1_tn_slope), mean(dfm$d1_tn_slope, na.rm = TRUE), sd(dfm$d1_tn_slope, na.rm = TRUE))
p4 <- rnorm(length(dfm$d2_tn_slope), mean(dfm$d2_tn_slope, na.rm = TRUE), sd(dfm$d2_tn_slope, na.rm = TRUE))


quartz()
png(filename = paste("rnorm_All_sumtxs_sum_slope.png"), width = 500, height= 500, res = 90)
hist (p1, prob=FALSE, breaks=10, col=rgb(0.5,0,0,0.4), xlab="Slope of summer Tx vs. time", ylim=c(0,100), main=NULL, xlim=c(-1.5,1), ylab = "Stations")
hist (p2, prob=FALSE, breaks=5, col=rgb(0,0,0.5,0.4), add=TRUE)
leg_txt <- c("1997-2006","2007-2016")
col_code <- c(rgb(0,0,0.5,0.4), rgb(0.5,0,0,0.4))
legend("left", legend = leg_txt, fill=col_code, horiz = FALSE, cex = 1, border="black", box.col = NULL, bty = "n")
dev.off()
graphics.off()  

quartz()
png(filename = paste("rnorm_All_sumtn_sum_slope.png"), width = 500, height= 500, res = 90)
hist (p3, prob=FALSE, breaks=5, col=rgb(0.5,0,0,0.4), xlab="Slope of summer Tn vs. time", ylim=c(0,100), main=NULL, xlim=c(-1.5,1), ylab = "Stations")
hist (p4, prob=FALSE,  breaks=5, col=rgb(0,0,0.5,0.4), add=TRUE)
leg_txt <- c("1997-2006","2007-2016")
col_code <- c(rgb(0,0,0.5,0.4), rgb(0.5,0,0,0.4))
legend("left", legend = leg_txt, fill=col_code, horiz = FALSE, cex = 1, border="black", box.col = NULL, bty = "n")
dev.off()
graphics.off()  


CW <- subset(dfm, region == "CW") # D1 as 2007-2016 including both years - 10y
N <- subset(dfm, region == "N") # D1 as 2007-2016 including both years - 10y
NE <- subset(dfm, region == "NE") # D1 as 2007-2016 including both years - 10y
S <- subset(dfm, region == "S") # D1 as 2007-2016 including both years - 10y
SE <- subset(dfm, region == "SE") # D1 as 2007-2016 including both years - 10y


#--------------- Central western SKIPPED only 5 stations -----------------


p1 <- rnorm(length(NE$d1_txs_slope), mean(NE$d1_txs_slope, na.rm = TRUE), sd(NE$d1_txs_slope, na.rm = TRUE))
p2 <- rnorm(length(NE$d2_txs_slope), mean(NE$d2_txs_slope, na.rm = TRUE), sd(NE$d2_txs_slope, na.rm = TRUE))
p3 <- rnorm(length(NE$d1_tn_slope), mean(NE$d1_tn_slope, na.rm = TRUE), sd(NE$d1_tn_slope, na.rm = TRUE))
p4 <- rnorm(length(NE$d2_tn_slope), mean(NE$d2_tn_slope, na.rm = TRUE), sd(NE$d2_tn_slope, na.rm = TRUE))




quartz()
png(filename = paste("rnorm_NE_sumtxs_sum_slope.png"), width = 500, height= 500, res = 90)
hist (p1, prob=FALSE, breaks=10, col=rgb(0.5,0,0,0.4), xlab="Slope of summer Tx vs. time", ylim=c(0,50), main=NULL, xlim=c(-1.5,1), ylab = "Stations")
hist (p2, prob=FALSE, breaks=5, col=rgb(0,0,0.5,0.4), add=TRUE)
leg_txt <- c("1997-2006","2007-2016")
text(-1.5,50, "Northeast region", cex=0.9, col="grey20", adj = 0)
col_code <- c(rgb(0,0,0.5,0.4), rgb(0.5,0,0,0.4))
legend("left", legend = leg_txt, fill=col_code, horiz = FALSE, cex = 1, border="black", box.col = NULL, bty = "n")
dev.off()
graphics.off()  

quartz()
png(filename = paste("rnorm_NE_sumtn_sum_slope.png"), width = 500, height= 500, res = 90)
hist (p3, prob=FALSE, breaks=5, col=rgb(0.5,0,0,0.4), xlab="Slope of summer Tn vs. time", ylim=c(0,50), main=NULL, xlim=c(-1.5,1), ylab = "Stations")
hist (p4, prob=FALSE,  breaks=5, col=rgb(0,0,0.5,0.4), add=TRUE)
leg_txt <- c("1997-2006","2007-2016")
col_code <- c(rgb(0,0,0.5,0.4), rgb(0.5,0,0,0.4))
legend("left", legend = leg_txt, fill=col_code, horiz = FALSE, cex = 1, border="black", box.col = NULL, bty = "n")
text(-1.5,50, "Northeast region", cex=0.9, col="grey20", adj = 0)
dev.off()
graphics.off()  


#--------------- North -----------------

p1 <- rnorm(length(N$d1_txs_slope), mean(N$d1_txs_slope, na.rm = TRUE), sd(N$d1_txs_slope, na.rm = TRUE))
p2 <- rnorm(length(N$d2_txs_slope), mean(N$d2_txs_slope, na.rm = TRUE), sd(N$d2_txs_slope, na.rm = TRUE))
p3 <- rnorm(length(N$d1_tn_slope), mean(N$d1_tn_slope, na.rm = TRUE), sd(N$d1_tn_slope, na.rm = TRUE))
p4 <- rnorm(length(N$d2_tn_slope), mean(N$d2_tn_slope, na.rm = TRUE), sd(N$d2_tn_slope, na.rm = TRUE))


quartz()
png(filename = paste("rnorm_N_sumtxs_sum_slope.png"), width = 500, height= 500, res = 90)
hist (p1, prob=FALSE, breaks=5, col=rgb(0.5,0,0,0.4), xlab="Slope of summer Tx vs. time", ylim=c(0,20), main=NULL, xlim=c(-1.5,1), ylab = "Stations")
hist (p2, prob=FALSE, breaks=5, col=rgb(0,0,0.5,0.4), add=TRUE)
leg_txt <- c("1997-2006","2007-2016")
text(-1.5,20, "North region", cex=0.9, col="grey20", adj = 0)
col_code <- c(rgb(0,0,0.5,0.4), rgb(0.5,0,0,0.4))
legend("left", legend = leg_txt, fill=col_code, horiz = FALSE, cex = 1, border="black", box.col = NULL, bty = "n")
dev.off()
graphics.off()  

quartz()
png(filename = paste("rnorm_N_sumtn_sum_slope.png"), width = 500, height= 500, res = 90)
hist (p3, prob=FALSE, breaks=10, col=rgb(0.5,0,0,0.4), xlab="Slope of summer Tn vs. time", ylim=c(0,20), main=NULL, xlim=c(-1.5,1), ylab = "Stations")
hist (p4, prob=FALSE,  breaks=3, col=rgb(0,0,0.5,0.4), add=TRUE)
leg_txt <- c("1997-2006","2007-2016")
col_code <- c(rgb(0,0,0.5,0.4), rgb(0.5,0,0,0.4))
legend("left", legend = leg_txt, fill=col_code, horiz = FALSE, cex = 1, border="black", box.col = NULL, bty = "n")
text(-1.5,20, "North region", cex=0.9, col="grey20", adj = 0)
dev.off()
graphics.off()  



#--------------- Southeast -----------------

p1 <- rnorm(length(SE$d1_txs_slope), mean(SE$d1_txs_slope, na.rm = TRUE), sd(SE$d1_txs_slope, na.rm = TRUE))
p2 <- rnorm(length(SE$d2_txs_slope), mean(SE$d2_txs_slope, na.rm = TRUE), sd(SE$d2_txs_slope, na.rm = TRUE))
p3 <- rnorm(length(SE$d1_tn_slope), mean(SE$d1_tn_slope, na.rm = TRUE), sd(SE$d1_tn_slope, na.rm = TRUE))
p4 <- rnorm(length(SE$d2_tn_slope), mean(SE$d2_tn_slope, na.rm = TRUE), sd(SE$d2_tn_slope, na.rm = TRUE))


quartz()
png(filename = paste("rnorm_SE_sumtxs_sum_slope.png"), width = 500, height= 500, res = 90)
hist (p1, prob=FALSE, breaks=10, col=rgb(0.5,0,0,0.4), xlab="Slope of summer Tx vs. time", ylim=c(0,35), main=NULL, xlim=c(-1.5,1), ylab = "Stations")
hist (p2, prob=FALSE, breaks=3, col=rgb(0,0,0.5,0.4), add=TRUE)
leg_txt <- c("1997-2006","2007-2016")
text(-1.5,35, "Southeast region", cex=0.9, col="grey20", adj = 0)
col_code <- c(rgb(0,0,0.5,0.4), rgb(0.5,0,0,0.4))
legend("left", legend = leg_txt, fill=col_code, horiz = FALSE, cex = 1, border="black", box.col = NULL, bty = "n")
dev.off()
graphics.off()  

quartz()
png(filename = paste("rnorm_SE_sumtn_sum_slope.png"), width = 500, height= 500, res = 90)
hist (p3, prob=FALSE, breaks=5, col=rgb(0.5,0,0,0.4), xlab="Slope of summer Tn vs. time", ylim=c(0,35), main=NULL, xlim=c(-1.5,1), ylab = "Stations")
hist (p4, prob=FALSE,  breaks=5, col=rgb(0,0,0.5,0.4), add=TRUE)
leg_txt <- c("1997-2006","2007-2016")
col_code <- c(rgb(0,0,0.5,0.4), rgb(0.5,0,0,0.4))
legend("left", legend = leg_txt, fill=col_code, horiz = FALSE, cex = 1, border="black", box.col = NULL, bty = "n")
text(-1.5,35, "Southeast region", cex=0.9, col="grey20", adj = 0)
dev.off()
graphics.off()  


#--------------- South -----------------

p1 <- rnorm(length(S$d1_txs_slope), mean(S$d1_txs_slope, na.rm = TRUE), sd(S$d1_txs_slope, na.rm = TRUE))
p2 <- rnorm(length(S$d2_txs_slope), mean(S$d2_txs_slope, na.rm = TRUE), sd(S$d2_txs_slope, na.rm = TRUE))
p3 <- rnorm(length(S$d1_tn_slope), mean(S$d1_tn_slope, na.rm = TRUE), sd(S$d1_tn_slope, na.rm = TRUE))
p4 <- rnorm(length(S$d2_tn_slope), mean(S$d2_tn_slope, na.rm = TRUE), sd(S$d2_tn_slope, na.rm = TRUE))


quartz()
png(filename = paste("rnorm_S_sumtxs_sum_slope.png"), width = 500, height= 500, res = 90)
hist (p1, prob=FALSE, breaks=5, col=rgb(0.5,0,0,0.4), xlab="Slope of summer Tx vs. time", ylim=c(0,15), main=NULL, xlim=c(-1.5,1), ylab = "Stations")
hist (p2, prob=FALSE, breaks=5, col=rgb(0,0,0.5,0.4), add=TRUE)
leg_txt <- c("1997-2006","2007-2016")
text(-1.5,15, "South region", cex=0.9, col="grey20", adj = 0)
col_code <- c(rgb(0,0,0.5,0.4), rgb(0.5,0,0,0.4))
legend("left", legend = leg_txt, fill=col_code, horiz = FALSE, cex = 1, border="black", box.col = NULL, bty = "n")
dev.off()
graphics.off()  

quartz()
png(filename = paste("rnorm_S_sumtn_sum_slope.png"), width = 500, height= 500, res = 90)
hist (p3, prob=FALSE, breaks=5, col=rgb(0.5,0,0,0.4), xlab="Slope of summer Tn vs. time", ylim=c(0,15), main=NULL, xlim=c(-1.5,1), ylab = "Stations")
hist (p4, prob=FALSE,  breaks=3, col=rgb(0,0,0.5,0.4), add=TRUE)
leg_txt <- c("1997-2006","2007-2016")
col_code <- c(rgb(0,0,0.5,0.4), rgb(0.5,0,0,0.4))
legend("left", legend = leg_txt, fill=col_code, horiz = FALSE, cex = 1, border="black", box.col = NULL, bty = "n")
text(-1.5,15, "South region", cex=0.9, col="grey20", adj = 0)
dev.off()
graphics.off()  



#--------------- Centralwestern -----------------

p1 <- rnorm(length(CW$d1_txs_slope), mean(CW$d1_txs_slope, na.rm = TRUE), sd(CW$d1_txs_slope, na.rm = TRUE))
p2 <- rnorm(length(CW$d2_txs_slope), mean(CW$d2_txs_slope, na.rm = TRUE), sd(CW$d2_txs_slope, na.rm = TRUE))
p3 <- rnorm(length(CW$d1_tn_slope), mean(CW$d1_tn_slope, na.rm = TRUE), sd(CW$d1_tn_slope, na.rm = TRUE))
p4 <- rnorm(length(CW$d2_tn_slope), mean(CW$d2_tn_slope, na.rm = TRUE), sd(CW$d2_tn_slope, na.rm = TRUE))


quartz()
png(filename = paste("rnorm_CW_sumtxs_sum_slope.png"), width = 500, height= 500, res = 90)
hist (p1, prob=FALSE, breaks=10, col=rgb(0.5,0,0,0.4), xlab="Slope of summer Tx vs. time", ylim=c(0,15), main=NULL, xlim=c(-1.5,1), ylab = "Stations")
hist (p2, prob=FALSE, breaks=2.5, col=rgb(0,0,0.5,0.4), add=TRUE)
leg_txt <- c("1997-2006","2007-2016")
text(-1.5,15, "Centralwestern region", cex=0.9, col="grey20", adj = 0)
col_code <- c(rgb(0,0,0.5,0.4), rgb(0.5,0,0,0.4))
legend("left", legend = leg_txt, fill=col_code, horiz = FALSE, cex = 1, border="black", box.col = NULL, bty = "n")
dev.off()
graphics.off()  

quartz()
png(filename = paste("rnorm_CW_sumtn_sum_slope.png"), width = 500, height= 500, res = 90)
hist (p3, prob=FALSE, breaks=6, col=rgb(0.5,0,0,0.4), xlab="Slope of summer Tn vs. time", ylim=c(0,15), main=NULL, xlim=c(-1.5,1), ylab = "Stations")
hist (p4, prob=FALSE,  breaks=3, col=rgb(0,0,0.5,0.4), add=TRUE)
leg_txt <- c("1997-2006","2007-2016")
col_code <- c(rgb(0,0,0.5,0.4), rgb(0.5,0,0,0.4))
legend("left", legend = leg_txt, fill=col_code, horiz = FALSE, cex = 1, border="black", box.col = NULL, bty = "n")
text(-1.5,15, "Centralwestern region", cex=0.9, col="grey20", adj = 0)
dev.off()
graphics.off()  



