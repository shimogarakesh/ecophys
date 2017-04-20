################################################################################################################################################################
# creating dataframe with monthly aggregates: one extra year before the time period of the data added 
# variables mapped to one year before the actual period it represents. 
# e.g., 2001 Jan-feb was considered as part of 2001 summer starting from December 2000  

library(plyr)
library(grid)
library(gridExtra)
library(zoo)
library(data.table)
# for loop to process all CSV files in a folder
# make a list of all files in a folder on terminal$ ls *.csv > filenames
# set working directory
setwd('/Volumes/Extended/INMET/looping_trial')

filenames = read.table('filenames')

nsites=dim(filenames)[1]

for (n in 1:nsites){ 
  #n=6
  file_n = filenames[n,]
  
  data_frame = read.csv(as.character(file_n))
 
####### start working on the loop here
  yrmax <- max(data_frame[,4], na.rm = T)
  yrmin <- min(data_frame[,4], na.rm = T)
  yeartot <- yrmax - yrmin + 1
  yrlist <- c(yrmin:yrmax)

  names(data_frame)[1] <- "station"
  names(data_frame)[2] <- "day"
  names(data_frame)[3] <- "month"
  names(data_frame)[4] <- "year"
  names(data_frame)[5] <- "hour"
  names(data_frame)[6] <- "prcp"
  names(data_frame)[7] <- "tmax"
  names(data_frame)[8] <- "tmin"
  names(data_frame)[9] <- "insul" # not sure what this variable is

# calculate seasonal averages using library(plyr)
# additional info: http://stackoverflow.com/questions/15105670/how-to-calculate-average-values-large-datasets

# anual aggergates
  tmax_ann_mn <- ddply(data_frame, .(year), summarise, tmax_ann_mn <- mean(tmax, na.rm = TRUE))
  names(tmax_ann_mn)[2] <- "tmax_ann_mean"

  tmin_ann_mn <- ddply(data_frame, .(year), summarise, tmin_ann_mn <- mean(tmin, na.rm = TRUE))
  names(tmin_ann_mn)[2] <- "tmin_ann_mean"

# using subset function to extract summer months: december, january and february

  jf <- subset(data_frame, month < 3, select=c(day, month, year, tmax, tmin, prcp))
  dec <- subset(data_frame, month > 11, select=c(day, month, year, tmax, tmin, prcp))

# calculate yearly aggregate - summer 
  sum_tmax_jf <- ddply(jf, .(year), summarise, sum_tmax_jf <- mean(tmax, na.rm = TRUE))
  sum_tmax_dec <- ddply(dec, .(year), summarise, sum_tmax_dec <- mean(tmax, na.rm = TRUE))

  sum_tmin_jf <- ddply(jf, .(year), summarise, sum_tmin_jf <- mean(tmin, na.rm = TRUE))
  sum_tmin_dec <- ddply(dec, .(year), summarise, sum_tmin_dec <- mean(tmin, na.rm = TRUE))

  sum_tmin_jf[,1] <- sum_tmin_jf[,1]-1  # subtracts year length by one - shifting the values by one year down
  sum_tmax_jf[,1] <- sum_tmax_jf[,1]-1  # subtracts year length by one - shifting the values by one year down
 

  # merge dataframes with irregular timeseries using library(zoo) 
  # reference: http://stackoverflow.com/questions/7089444/r-merge-two-irregular-time-series
  
  sum_tmax_dec <- na.omit(sum_tmax_dec)
  sum_tmax_jf <- na.omit(sum_tmax_jf)
  sum_tmin_dec <- na.omit(sum_tmin_dec)
  sum_tmin_jf <- na.omit(sum_tmin_jf)
  tmax_ann_mn <- na.omit(tmax_ann_mn)
  tmin_ann_mn <- na.omit(tmin_ann_mn)
  
  xx <-  read.zoo(sum_tmax_dec)
  yy <-  read.zoo(sum_tmax_jf)
  zz <-  read.zoo(sum_tmin_dec)
  aa <-  read.zoo(sum_tmin_jf)
  bb <- read.zoo(tmax_ann_mn) 
  cc <- read.zoo(tmin_ann_mn)

  df <- merge.zoo(xx, yy, zz, aa, bb, cc)  
  df <- as.data.frame(df, row.names = NULL)
  
# converts zoo object to dataframe again
# the output dataframe uses year as row names. to extract row names as a variable vector
# use library(data.table) setDT function http://stackoverflow.com/questions/29511215/convert-row-names-into-first-column
# this adds an extra row at the end that needs deletion
# Aggergating all summer aggregate columns

  df <- setDT(df, keep.rownames = TRUE)

  df$sum_tmax_mean <- rowMeans(subset(df, select = c(xx, yy)), na.rm = TRUE)  
  df$summ_tmin_mean <- rowMeans(subset(df, select = c(zz, aa)), na.rm = TRUE)  
  
  df$xx <- NULL
  df$yy <- NULL
  df$zz <- NULL
  df$aa <- NULL
  df$rn <- as.numeric(df$rn)


  df$summ_tmax_anom <- df$sum_tmax_mean - mean(df$sum_tmax_mean, na.rm = TRUE)
  df$summ_tmin_anom <- df$summ_tmin_mean - mean(df$summ_tmin_mean, na.rm = TRUE)
  
  names(df)[1] <- "year"
  names(df)[2] <- "ann_mean_tmax"
  names(df)[3] <- "ann_mean_tmin"
  tstr <- substr(as.character(file_n), 1, nchar(as.character(file_n))-10)
  
  df$ann_tmax_anom <- df$ann_mean_tmax - mean(df$ann_mean_tmax, na.rm = TRUE)
  df$ann_tmin_anom <- df$ann_mean_tmin - mean(df$ann_mean_tmin, na.rm = TRUE)
  df$station <- rep(tstr, )
  
  tminmin <- min(df$summ_tmin_anom, na.rm = T)
  tmaxmax <- max(df$summ_tmax_anom, na.rm = T)

  quartz()
  png(filename = paste(tstr,"summ_anom.png"), width = 900, height= 500, res = 90)
  plot(y=df$summ_tmax_anom, x=df$year, type = "l", col="red", xlab = "", xlim = c(yrmin,yrmax),ylab = expression("Summer temperature anomaly"~degree~C), ylim=c(-3,3), sub =tstr, main=NULL)
  lines(y=df$summ_tmin_anom, x=df$year, col="blue")
  abline(h=0, col="grey")
  leg_txt <- c("maximum", "minimum")
  col_code <- c("red", "blue")
  legend("topleft", legend = leg_txt, fill=col_code, horiz = TRUE, cex = 1, border="white", box.col = NULL, bty = "n")
  dev.off()

  quartz()
  png(filename = paste(tstr,"annual_anom.png"), width = 900, height= 500, res = 90)
  plot(y=df$ann_tmax_anom, x=df$year, type = "l", col="red", xlab = "", xlim = c(yrmin,yrmax),ylab = expression("Annual temperature anomaly"~degree~C), ylim=c(-3,3), sub =tstr, main=NULL)
  lines(y=df$ann_tmin_anom, x=df$year, col="blue")
  abline(h=0, col="grey")
  leg_txt <- c("maximum", "minimum")
  col_code <- c("red", "blue")
  legend("topleft", legend = leg_txt, fill=col_code, horiz = TRUE, cex = 1, border="white", box.col = NULL, bty = "n")
  dev.off()
  
  write.table(df, file="output.csv", eol = "\r", na = "NA", row.names = FALSE, append = TRUE, sep=",", col.names = !file.exists("output.csv"))
  
}





# write.csv(df, file=paste("out",tstr,".csv"), eol = "\r", na = "NA", row.names = FALSE)
# slope of a chunk of dataframe http://stackoverflow.com/questions/31059043/how-can-i-calculate-the-slope-of-multiple-subsets-of-a-data-frame-more-efficient


