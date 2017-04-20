
setwd("//Volumes/Extended/INMET/allstations/")

dfm <- read.csv("percentile_occ.csv", header = TRUE, sep = ",")

p1 <- rnorm(length(dfm$d1_tx90), mean(dfm$d1_tx90, na.rm = TRUE), sd(dfm$d1_tx90, na.rm = TRUE))
p2 <- rnorm(length(dfm$d2_tx90), mean(dfm$d2_tx90, na.rm = TRUE), sd(dfm$d2_tx90, na.rm = TRUE))
p3 <- rnorm(length(dfm$d1_tn90), mean(dfm$d1_tn90, na.rm = TRUE), sd(dfm$d1_tn90, na.rm = TRUE))
p4 <- rnorm(length(dfm$d2_tn90), mean(dfm$d2_tn90, na.rm = TRUE), sd(dfm$d2_tn90, na.rm = TRUE))


quartz()
png(filename = paste("rnorm_All_sumtx90p.png"), width = 500, height= 500, res = 90)
hist (p1, prob=FALSE, breaks=20, col=rgb(0.5,0,0,0.4), xlab="Days with Tx >90p", ylim=c(0,60), main=NULL, xlim=c(0,1200), ylab = "Stations")
hist (p2, prob=FALSE, breaks=10, col=rgb(0,0,0.5,0.4), add=TRUE)
leg_txt <- c("1997-2006","2007-2016")
col_code <- c(rgb(0,0,0.5,0.4), rgb(0.5,0,0,0.4))
legend("topright", legend = leg_txt, fill=col_code, horiz = FALSE, cex = 1, border="black", box.col = NULL, bty = "n")
dev.off()
graphics.off()  

quartz()
png(filename = paste("rnorm_All_sumtn90p.png"), width = 500, height= 500, res = 90)
hist (p3, prob=FALSE, breaks=20, col=rgb(0.5,0,0,0.4), xlab="Days with Tn >90p", ylim=c(0,60), main=NULL, xlim=c(0,1200), ylab = "Stations")
hist (p4, prob=FALSE,  breaks=20, col=rgb(0,0,0.5,0.4), add=TRUE)
leg_txt <- c("1997-2006","2007-2016")
col_code <- c(rgb(0,0,0.5,0.4), rgb(0.5,0,0,0.4))
legend("topright", legend = leg_txt, fill=col_code, horiz = FALSE, cex = 1, border="black", box.col = NULL, bty = "n")
dev.off()
graphics.off()  


CW <- subset(dfm, Region == "CW") # D1 as 2007-2016 including both years - 10y
N <- subset(dfm, Region == "N") # D1 as 2007-2016 including both years - 10y
NE <- subset(dfm, Region == "NE") # D1 as 2007-2016 including both years - 10y
S <- subset(dfm, Region == "S") # D1 as 2007-2016 including both years - 10y
SE <- subset(dfm, Region == "SE") # D1 as 2007-2016 including both years - 10y


#--------------- Central western SKIPPED only 5 stations -----------------


p1 <- rnorm(length(NE$d1_tx90), mean(NE$d1_tx90, na.rm = TRUE), sd(NE$d1_tx90, na.rm = TRUE))
p2 <- rnorm(length(NE$d2_tx90), mean(NE$d2_tx90, na.rm = TRUE), sd(NE$d2_tx90, na.rm = TRUE))
p3 <- rnorm(length(NE$d1_tn90), mean(NE$d1_tn90, na.rm = TRUE), sd(NE$d1_tn90, na.rm = TRUE))
p4 <- rnorm(length(NE$d2_tn90), mean(NE$d2_tn90, na.rm = TRUE), sd(NE$d2_tn90, na.rm = TRUE))




quartz()
png(filename = paste("rnorm_NE_sumtx90p.png"), width = 500, height= 500, res = 90)
hist (p1, prob=FALSE, breaks=10, col=rgb(0.5,0,0,0.4), xlab="Days with Tx >90p", ylim=c(0,25), main=NULL, xlim=c(0,1200), ylab = "Stations")
hist (p2, prob=FALSE, breaks=10, col=rgb(0,0,0.5,0.4), add=TRUE)
leg_txt <- c("1997-2006","2007-2016")
text(-1,25, "Northeast region", cex=0.9, col="grey20", adj = 0)
col_code <- c(rgb(0,0,0.5,0.4), rgb(0.5,0,0,0.4))
legend("topright", legend = leg_txt, fill=col_code, horiz = FALSE, cex = 1, border="black", box.col = NULL, bty = "n")
dev.off()
graphics.off()  

quartz()
png(filename = paste("rnorm_NE_sumtn90p.png"), width = 500, height= 500, res = 90)
hist (p3, prob=FALSE, breaks=20, col=rgb(0.5,0,0,0.4), xlab="Days with Tn >90p", ylim=c(0,25), main=NULL, xlim=c(0,1200), ylab = "Stations")
hist (p4, prob=FALSE,  breaks=20, col=rgb(0,0,0.5,0.4), add=TRUE)
leg_txt <- c("1997-2006","2007-2016")
col_code <- c(rgb(0,0,0.5,0.4), rgb(0.5,0,0,0.4))
legend("topright", legend = leg_txt, fill=col_code, horiz = FALSE, cex = 1, border="black", box.col = NULL, bty = "n")
text(-1,25, "Northeast region", cex=0.9, col="grey20", adj = 0)
dev.off()
graphics.off()  


#--------------- North -----------------

p1 <- rnorm(length(N$d1_tx90), mean(N$d1_tx90, na.rm = TRUE), sd(N$d1_tx90, na.rm = TRUE))
p2 <- rnorm(length(N$d2_tx90), mean(N$d2_tx90, na.rm = TRUE), sd(N$d2_tx90, na.rm = TRUE))
p3 <- rnorm(length(N$d1_tn90), mean(N$d1_tn90, na.rm = TRUE), sd(N$d1_tn90, na.rm = TRUE))
p4 <- rnorm(length(N$d2_tn90), mean(N$d2_tn90, na.rm = TRUE), sd(N$d2_tn90, na.rm = TRUE))


quartz()
png(filename = paste("rnorm_N_sumtx90p.png"), width = 500, height= 500, res = 90)
hist (p1, prob=FALSE, breaks=10, col=rgb(0.5,0,0,0.4), xlab="Days with Tx >90p", ylim=c(0,10), main=NULL, xlim=c(0,1200), ylab = "Stations")
hist (p2, prob=FALSE, breaks=10, col=rgb(0,0,0.5,0.4), add=TRUE)
leg_txt <- c("1997-2006","2007-2016")
text(-1,10, "North region", cex=0.9, col="grey20", adj = 0)
col_code <- c(rgb(0,0,0.5,0.4), rgb(0.5,0,0,0.4))
legend("topright", legend = leg_txt, fill=col_code, horiz = FALSE, cex = 1, border="black", box.col = NULL, bty = "n")
dev.off()
graphics.off()  

quartz()
png(filename = paste("rnorm_N_sumtn90p.png"), width = 500, height= 500, res = 90)
hist (p3, prob=FALSE, breaks=10, col=rgb(0.5,0,0,0.4), xlab="Days with Tn >90p", ylim=c(0,10), main=NULL, xlim=c(0,1200), ylab = "Stations")
hist (p4, prob=FALSE,  breaks=10, col=rgb(0,0,0.5,0.4), add=TRUE)
leg_txt <- c("1997-2006","2007-2016")
col_code <- c(rgb(0,0,0.5,0.4), rgb(0.5,0,0,0.4))
legend("topright", legend = leg_txt, fill=col_code, horiz = FALSE, cex = 1, border="black", box.col = NULL, bty = "n")
text(-1,10, "North region", cex=0.9, col="grey20", adj = 0)
dev.off()
graphics.off()  



#--------------- Southeast -----------------

p1 <- rnorm(length(SE$d1_tx90), mean(SE$d1_tx90, na.rm = TRUE), sd(SE$d1_tx90, na.rm = TRUE))
p2 <- rnorm(length(SE$d2_tx90), mean(SE$d2_tx90, na.rm = TRUE), sd(SE$d2_tx90, na.rm = TRUE))
p3 <- rnorm(length(SE$d1_tn90), mean(SE$d1_tn90, na.rm = TRUE), sd(SE$d1_tn90, na.rm = TRUE))
p4 <- rnorm(length(SE$d2_tn90), mean(SE$d2_tn90, na.rm = TRUE), sd(SE$d2_tn90, na.rm = TRUE))


quartz()
png(filename = paste("rnorm_SE_sumtx90p.png"), width = 500, height= 500, res = 90)
hist (p1, prob=FALSE, breaks=10, col=rgb(0.5,0,0,0.4), xlab="Days with Tx >90p", ylim=c(0,30), main=NULL, xlim=c(0,1200), ylab = "Stations")
hist (p2, prob=FALSE, breaks=10, col=rgb(0,0,0.5,0.4), add=TRUE)
leg_txt <- c("1997-2006","2007-2016")
text(-1,30, "Southeast region", cex=0.9, col="grey20", adj = 0)
col_code <- c(rgb(0,0,0.5,0.4), rgb(0.5,0,0,0.4))
legend("topright", legend = leg_txt, fill=col_code, horiz = FALSE, cex = 1, border="black", box.col = NULL, bty = "n")
dev.off()
graphics.off()  

quartz()
png(filename = paste("rnorm_SE_sumtn90p.png"), width = 500, height= 500, res = 90)
hist (p3, prob=FALSE, breaks=10, col=rgb(0.5,0,0,0.4), xlab="Days with Tn >90p", ylim=c(0,30), main=NULL, xlim=c(0,1200), ylab = "Stations")
hist (p4, prob=FALSE,  breaks=10, col=rgb(0,0,0.5,0.4), add=TRUE)
leg_txt <- c("1997-2006","2007-2016")
col_code <- c(rgb(0,0,0.5,0.4), rgb(0.5,0,0,0.4))
legend("topright", legend = leg_txt, fill=col_code, horiz = FALSE, cex = 1, border="black", box.col = NULL, bty = "n")
text(-1,30, "Southeast region", cex=0.9, col="grey20", adj = 0)
dev.off()
graphics.off()  


#--------------- South -----------------

p1 <- rnorm(length(S$d1_tx90), mean(S$d1_tx90, na.rm = TRUE), sd(S$d1_tx90, na.rm = TRUE))
p2 <- rnorm(length(S$d2_tx90), mean(S$d2_tx90, na.rm = TRUE), sd(S$d2_tx90, na.rm = TRUE))
p3 <- rnorm(length(S$d1_tn90), mean(S$d1_tn90, na.rm = TRUE), sd(S$d1_tn90, na.rm = TRUE))
p4 <- rnorm(length(S$d2_tn90), mean(S$d2_tn90, na.rm = TRUE), sd(S$d2_tn90, na.rm = TRUE))


quartz()
png(filename = paste("rnorm_S_sumtx90p.png"), width = 500, height= 500, res = 90)
hist (p1, prob=FALSE, breaks=10, col=rgb(0.5,0,0,0.4), xlab="Days with Tx >90p", ylim=c(0,15), main=NULL, xlim=c(0,1200), ylab = "Stations")
hist (p2, prob=FALSE, breaks=5, col=rgb(0,0,0.5,0.4), add=TRUE)
leg_txt <- c("1997-2006","2007-2016")
text(-1,15, "South region", cex=0.9, col="grey20", adj = 0)
col_code <- c(rgb(0,0,0.5,0.4), rgb(0.5,0,0,0.4))
legend("topright", legend = leg_txt, fill=col_code, horiz = FALSE, cex = 1, border="black", box.col = NULL, bty = "n")
dev.off()
graphics.off()  

quartz()
png(filename = paste("rnorm_S_sumtn90p.png"), width = 500, height= 500, res = 90)
hist (p3, prob=FALSE, breaks=10, col=rgb(0.5,0,0,0.4), xlab="Days with Tn >90p", ylim=c(0,15), main=NULL, xlim=c(0,1200), ylab = "Stations")
hist (p4, prob=FALSE,  breaks=5, col=rgb(0,0,0.5,0.4), add=TRUE)
leg_txt <- c("1997-2006","2007-2016")
col_code <- c(rgb(0,0,0.5,0.4), rgb(0.5,0,0,0.4))
legend("topright", legend = leg_txt, fill=col_code, horiz = FALSE, cex = 1, border="black", box.col = NULL, bty = "n")
text(-1,15, "South region", cex=0.9, col="grey20", adj = 0)
dev.off()
graphics.off()  



#--------------- Centralwestern -----------------

p1 <- rnorm(length(CW$d1_tx90), mean(CW$d1_tx90, na.rm = TRUE), sd(CW$d1_tx90, na.rm = TRUE))
p2 <- rnorm(length(CW$d2_tx90), mean(CW$d2_tx90, na.rm = TRUE), sd(CW$d2_tx90, na.rm = TRUE))
p3 <- rnorm(length(CW$d1_tn90), mean(CW$d1_tn90, na.rm = TRUE), sd(CW$d1_tn90, na.rm = TRUE))
p4 <- rnorm(length(CW$d2_tn90), mean(CW$d2_tn90, na.rm = TRUE), sd(CW$d2_tn90, na.rm = TRUE))


quartz()
png(filename = paste("rnorm_CW_sumtx90p.png"), width = 500, height= 500, res = 90)
hist (p1, prob=FALSE, breaks=5, col=rgb(0.5,0,0,0.4), xlab="Days with Tx >90p", ylim=c(0,15), main=NULL, xlim=c(0,1200), ylab = "Stations")
hist (p2, prob=FALSE, breaks=5, col=rgb(0,0,0.5,0.4), add=TRUE)
leg_txt <- c("1997-2006","2007-2016")
text(-1,15, "Centralwestern region", cex=0.9, col="grey20", adj = 0)
col_code <- c(rgb(0,0,0.5,0.4), rgb(0.5,0,0,0.4))
legend("topright", legend = leg_txt, fill=col_code, horiz = FALSE, cex = 1, border="black", box.col = NULL, bty = "n")
dev.off()
graphics.off()  

quartz()
png(filename = paste("rnorm_CW_sumtn90p.png"), width = 500, height= 500, res = 90)
hist (p3, prob=FALSE, breaks=5, col=rgb(0.5,0,0,0.4), xlab="Days with Tn >90p", ylim=c(0,15), main=NULL, xlim=c(0,1200), ylab = "Stations")
hist (p4, prob=FALSE,  breaks=5, col=rgb(0,0,0.5,0.4), add=TRUE)
leg_txt <- c("1997-2006","2007-2016")
col_code <- c(rgb(0,0,0.5,0.4), rgb(0.5,0,0,0.4))
legend("topright", legend = leg_txt, fill=col_code, horiz = FALSE, cex = 1, border="black", box.col = NULL, bty = "n")
text(-1,15, "Centralwestern region", cex=0.9, col="grey20", adj = 0)
dev.off()
graphics.off()  



