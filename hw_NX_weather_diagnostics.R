# Nova Xavantina site weather data plots

library(lubridate)
library(dplyr)
library(TTR) # for simple moving average SMA()
library(data.table)
library(xts)

setwd("/Volumes/Extended/NX/Nova_Xavantina_2016/")
sub0 = read.table("preset1_200916.TXT",sep = "\t", skip=3, header = TRUE, stringsAsFactors = FALSE)
names(sub0)[1] = "date_time" # remane header

sub0$date_time2 = as.POSIXct(sub0$date_time, "%Y-%m-%d %I:%M", tz="America/Manaus")

# the line above does not work with wrong timezone
# http://rfunction.com/archives/1912
# http://neondataskills.org/R/time-series-subset-dplyr/
# https://www.stat.berkeley.edu/~s133/dates.html
# http://rstudio-pubs-static.s3.amazonaws.com/7415_a5c3c312c0204d7fbd9131bef15ce724.html
# http://biostat.mc.vanderbilt.edu/wiki/pub/Main/ColeBeck/datestimes.pdf
#timezone reference: https://en.wikipedia.org/wiki/List_of_tz_database_time_zones

plot(sub0$time, sub0$PAR, type = "l")
sub1 = sub0[sub0$date_time > as.POSIXct("2016-09-18"),]
sub1 = sub1[sub1$date_time < as.POSIXct("2016-09-19"),]
head(sub1)
#sub0$timelt = as.POSIXlt(sub0$time,format="%Y/%m/%d %H:%M:%S")
# add another column with POSIXlt

plot.ts(sub1$date_time, sub1$PAR, type="l")

sub2 = data.table(sub1)

str(sub2)

sub_par = sub2[,list(avg=mean(PAR)), by=hour(as.POSIXct(sub2$date_time, format="%Y/%m/%d %H:%M:%S"))]

sub_srd = sub2[,list(avg=mean(SRD)), by=hour(as.POSIXct(sub2$date_time, format="%Y/%m/%d %H:%M:%S"))]

sub_tmp = sub2[,list(avg=mean(TMP)), by=hour(as.POSIXct(sub2$date_time, format="%Y/%m/%d %H:%M:%S"))]

sub_hmd = sub2[,list(avg=mean(HMD)), by=hour(as.POSIXct(sub2$date_time, format="%Y/%m/%d %H:%M:%S"))]


plot(sub_tmp$hour, sub_tmp$avg, type="l", ylab = "micro mol/m2/s", xlab = "time(h)")

plot(sub_hmd$hour, sub_hmd$avg, type="l")

lines(sub_srd$hour, sub_srd$avg)

write.table(sub_srd, file="Solar_rad_hourly_18-sept-2016_NX.txt", sep="\t")
write.table(sub_par, file="Par_hourly_18-sept-2016_NX.txt", sep="\t")
write.table(sub_tmp, file="tmp_hourly_18-sept-2016_NX.txt", sep="\t")
write.table(sub_hmd, file="hmd_hourly_18-sept-2016_NX.txt", sep="\t")

