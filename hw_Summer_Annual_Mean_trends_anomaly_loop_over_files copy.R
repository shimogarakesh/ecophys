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
setwd('/Volumes/Extended/INMET/allstations/')

filenames = read.table('filenames')

nsites=dim(filenames)[1]

n = 5
for (n in 1:nsites){

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
  Tx_annual <- ddply(data_frame, .(year), summarise, Tx_annual <- mean(tmax, na.rm = TRUE))
  Tx_annual <- na.omit(Tx_annual)
  names(Tx_annual)[2] <- "Tx_annual"
  Tx_annual$Tx_annual_anom <- Tx_annual$Tx_annual - mean(Tx_annual$Tx_annual, na.rm = TRUE)


  Tn_annual <- ddply(data_frame, .(year), summarise, Tn_annual <- mean(tmin, na.rm = TRUE))
  Tn_annual <- na.omit(Tn_annual)
  names(Tn_annual)[2] <- "Tn_annual"
  Tn_annual$Tn_annual_anom <- Tn_annual$Tn_annual - mean(Tn_annual$Tn_annual, na.rm = TRUE)


# 90th percentile subsets: subset of dataframe with tmax and tmin > 90th percentile respectively

  Tx90q <- quantile(data_frame[,7], 0.90, na.rm = TRUE) # 90th percentile tmax
  Tn90q <- quantile(data_frame[,8], 0.90, na.rm = TRUE) # 90th percentile tmin

  Tx90p <- data_frame[data_frame[,"tmax"] > Tx90q,] # dataframe with tmax > 90th percentile
  Tn90p <- data_frame[data_frame[,"tmin"] > Tn90q,] # dataframe with tmax > 90th percentile

  Tx90p_count <- ddply(Tx90p, .(year), summarise, Tx90p_count <- length(tmax))
  Tx90p_count <- na.omit(Tx90p_count)
  names(Tx90p_count)[2] <- "Tx90p_count"


  Tn90p_count <- ddply(Tn90p, .(year), summarise, Tn90p_count <- length(tmin))
  Tn90p_count <- na.omit(Tn90p_count)
  names(Tn90p_count)[2] <- "Tn90p_count"


# using subset function to extract summer months: december, january and february

  jf <- subset(data_frame, month < 3, select=c(day, month, year, tmax, tmin, prcp))
  dec <- subset(data_frame, month > 11, select=c(day, month, year, tmax, tmin, prcp))

# summer averages
  Tx_sum_jf <- ddply(jf, .(year), summarise, Tx_sum_jf <- mean(tmax, na.rm = TRUE))
  Tx_sum_jf <- na.omit(Tx_sum_jf)
  names(Tx_sum_jf)[2] <- "Tx_sum_jf"

  Tx_sum_dec <- ddply(dec, .(year), summarise, Tx_sum_dec <- mean(tmax, na.rm = TRUE))
  Tx_sum_dec <- na.omit(Tx_sum_dec)
  names(Tx_sum_dec)[2] <- "Tx_sum_dec"

  Tn_sum_jf <- ddply(jf, .(year), summarise, Tn_sum_jf <- mean(tmin, na.rm = TRUE))
  Tn_sum_jf <- na.omit(Tn_sum_jf)
  names(Tn_sum_jf)[2] <- "Tn_sum_jf"

  Tn_sum_dec <- ddply(dec, .(year), summarise, Tn_sum_dec <- mean(tmin, na.rm = TRUE))
  Tn_sum_dec <- na.omit(Tn_sum_dec)
  names(Tn_sum_dec)[2] <- "Tn_sum_dec"

  Tn_sum_jf[,1] <- Tn_sum_jf[,1]-1  # subtracts year length by one - shifting the values by one year down
  Tx_sum_jf[,1] <- Tx_sum_jf[,1]-1  # subtracts year length by one - shifting the values by one year down


  # merge dataframes with irregular timeseries using library(zoo)
  # reference: http://stackoverflow.com/questions/7089444/r-merge-two-irregular-time-series


  xx <-  read.zoo(Tx_sum_dec)
  yy <-  read.zoo(Tx_sum_jf)
  zz <-  read.zoo(Tn_sum_dec)
  aa <-  read.zoo(Tn_sum_jf)
  bb <- read.zoo(Tx_annual)
  cc <- read.zoo(Tn_annual)
  dd <- read.zoo(Tx90p_count)
  ee <- read.zoo(Tn90p_count)


  df <- merge.zoo(xx, yy, zz, aa, bb, cc, dd, ee)
  df <- as.data.frame(df, row.names = NULL)

# converts zoo object to dataframe again
# the output dataframe uses year as row names. to extract row names as a variable vector
# use library(data.table) setDT function http://stackoverflow.com/questions/29511215/convert-row-names-into-first-column
# this adds an extra row at the end that needs deletion

  df <- setDT(df, keep.rownames = TRUE)

  df$Tx_sum <- rowMeans(subset(df, select = c(xx, yy)), na.rm = TRUE)
  df$Tn_sum <- rowMeans(subset(df, select = c(zz, aa)), na.rm = TRUE)

  df$xx <- NULL
  df$yy <- NULL
  df$zz <- NULL
  df$aa <- NULL
  df$rn <- as.numeric(df$rn)


  df$Tx_sum_anom <- df$Tx_sum - mean(df$Tx_sum, na.rm = TRUE)
  df$Tn_sum_anom <- df$Tn_sum - mean(df$Tn_sum, na.rm = TRUE)

  tmax_sum <- mean(df$Tx_sum, na.rm = TRUE)
  tmax_sum <- round(tmax_sum, 2)

  tmax_ann <- mean(df$Tx, na.rm = TRUE)
  tmax_ann <- round(tmax_ann, 2)

  tmin_sum <- mean(df$Tn_sum, na.rm = TRUE)
  tmin_sum <- round(tmin_sum, 2)

  tmin_ann <- mean(df$Tn, na.rm = TRUE)
  tmin_ann <- round(tmin_ann, 2)


    summary(df)


#  Tx_annual$Tx_annual_anom <- Tx_annual$Tx_annual - mean(Tx_annual$Tx_annual, na.rm = TRUE)
#  df$Tn_anom <- df$ann_mean_tmin - mean(df$ann_mean_tmin, na.rm = TRUE)

  names(df)[1] <- "year"
  names(df)[2] <- "Tx"
  names(df)[3] <- "Tx_anom"
  names(df)[4] <- "Tn"
  names(df)[5] <- "Tn_anom"
  names(df)[6] <- "Tx90p_count"
  names(df)[7] <- "Tn90p_count"

  tstr <- substr(as.character(file_n), 1, nchar(as.character(file_n))-10)
  df$station <- tstr
  df$Tx90p_value <- Tx90q
  df$Tn90p_value <- Tn90q

#  write.table(df, file="output.csv", eol = "\r", na = "NA", row.names = FALSE, append = TRUE, sep=",", col.names = !file.exists("output.csv"))

  df_plot <- df
  df_plot$Tx <- NULL
  df_plot$Tn <- NULL
  df_plot$Tx90p_count <- NULL
  df_plot$Tx90p_value <- NULL
  df_plot$Tn90p_count <- NULL
  df_plot$Tn90p_value <- NULL
  df_plot$station <- NULL
  df_plot$Tn_sum <- NULL


  x1 <- max(max(df$Tx90p_count, na.rm = TRUE), max(df$Tn90p_count, na.rm = TRUE))

  #txr2 <- summary(lm(df$Tx90p_count ~ df$year))$r.squared
  #tnr2 <- summary(lm(df$Tn90p_count ~ df$year))$r.squared
  txslope <- summary(lm(df$Tx90p_count ~ df$year))$coefficients[2,1]
  tnslope <- summary(lm(df$Tn90p_count ~ df$year))$coefficients[2,1]
  txslope <- round(txslope, 3)
  tnslope <- round(tnslope, 3)


  quartz()
  png(filename = paste(tstr,"_anom.png"), width = 900, height= 500, res = 90)
  plot(y=df_plot$Tx_sum_anom, x=df_plot$year, type = "l", col="red", xlab = "", xlim = c(yrmin,yrmax),ylab = expression("Temperature anomaly"~degree~C), main=NULL, ylim = c(-5, 5), cex.main = 0.8)
  lines(y=df_plot$Tx_anom, x=df_plot$year, col="red", lty="dotted", lwd = 1.5)
  lines(y=df_plot$Tn_sum_anom, x=df_plot$year, col="blue")
  lines(y=df_plot$Tn_anom, x=df_plot$year, col="blue", lty="dotted", lwd = 1.5)
  abline(h=0, col="grey")
  abline(v = 2016, col = "gray90")
  leg_txt <- c("maximum", "minimum")
  col_code <- c("red", "blue")
  legend(((min(df_plot$year))-0.5), -4, adj=0, legend = leg_txt, fill=col_code, horiz = TRUE, cex = 0.8, border="white", box.col = NULL, bty = "n")
  text(((min(df_plot$year))+0.5), -5, adj=0,  "solid lines = summer, dotted = annual", cex=0.8, col="grey30")
  text((min(df_plot$year)), 4.9, tstr, cex=0.9, col="grey20", adj = 0)
  text((min(df_plot$year)), 4.4, paste0(c("Tx annual mean : "), tmax_ann), cex=0.8, col="grey50", adj = 0)
  text((min(df_plot$year)), 4.0, paste0(c("Tx summer mean : "), tmax_sum), cex=0.8, col="grey50", adj = 0)
  text((min(df_plot$year)), 3.6, paste0(c("Tn annual mean : "), tmin_ann), cex=0.8, col="grey50", adj = 0)
  text((min(df_plot$year)), 3.2, paste0(c("Tn summer mean : "), tmin_sum), cex=0.8, col="grey50", adj = 0)
  dev.off()


  quartz()
  png(filename = paste(tstr,"_90p.png"), width = 900, height= 500, res = 90)
  plot(y=df$Tx90p_count, x=df$year, type = "p", pch = 19, col="red",  fill="red", xlab = "", xlim = c(yrmin,yrmax),ylab = "Temperature >90p (count)", main=NULL, ylim = c(0, x1), cex.main = 0.8)
  points(y=df$Tn90p_count, x=df$year, pch = 19, col="blue", fill="blue")
  abline(lm(df$Tx90p_count ~ df$year), col="red", lty="dotted", lwd=2)
  abline(lm(df$Tn90p_count ~ df$year), col="blue", lty="dotted", lwd=2)
  abline(v = 2016, col = "gray90")
  text((min(df$year, na.rm = T)), x1, tstr, cex=0.9, col="grey20", adj = 0)
  leg_txt <- c("maximum", "minimum")
  col_code <- c("red", "blue")
  mtext(paste0(c("Tx slope : "), txslope), side = 3, line = -2, at = (min(df$year, na.rm = T)), cex=0.8, col="red", adj = 0)
  mtext(paste0(c("Tn slope : "), tnslope), side = 3, line = -3, at = (min(df$year, na.rm = T)), cex=0.8, col="blue", adj = 0)
  # legend("bottomright", legend = c("maximum", "minimum"), pch = c(16,16),  fill=c("red", "blue"), horiz = FALSE, cex = 1, border="white", box.col = NULL, bty = "n")
  legend("top", cex = 0.8, bty = "n", legend = c("maximum", "minimum"), text.col = c("red", "blue"), col = c("red", "blue"), pch = c(19,19), horiz = TRUE)
  dev.off()
  graphics.off()

  #write.table(df, file="output.csv", eol = "\r", na = "NA", row.names = FALSE, append = TRUE, sep=",", col.names = !file.exists("output.csv"))

}

# need to add the slope over 2016-2006 and 2005-1995
# write.csv(df, file=paste("out",tstr,".csv"), eol = "\r", na = "NA", row.names = FALSE)
# slope of a chunk of dataframe http://stackoverflow.com/questions/31059043/how-can-i-calculate-the-slope-of-multiple-subsets-of-a-data-frame-more-efficient
# write.table(df, file="output.csv", eol = "\r", na = "NA", row.names = FALSE, append = TRUE, sep=",", col.names = !file.exists("output.csv"))
