# growth chamber arduino datalogger trials
# Snijder2 March 14 2017

setwd("/Volumes/Extended/Arduino-TC/")
data.frame = read.csv("371715.CSV")
data.frame$time = paste(data.frame$year,"-",data.frame$month,"-",data.frame$day," ",data.frame$hour,":",data.frame$minute,":",data.frame$second,sep="")


data.frame$time = as.POSIXct(data.frame$time, "%Y-%m-%d %I:%M:%s")

plot(data.frame$time, data.frame$room_temp_1, type="l", xlab="", ylab="temp_diff_1")#, ylim=c(-20,30))#, xlim=c(as.POSIXct("2017-03-10 23:59:59"), max(as.POSIXct("2017-03-13 01:12:00"))))



plot(data.frame$time, data.frame$room_temp_2, type="l", xlab="")#, ylab="temp_diff_1")#, ylim=c(-20,30))#, xlim=c(as.POSIXct("2017-03-10 23:59:59"), max(as.POSIXct("2017-03-13 01:12:00"))))








tail(data.frame)


sub1 = data.frame[data.frame$time > as.POSIXct("2017-03-07 23:59:00"),]
sub1 = data.frame[data.frame$time < as.POSIXct("2017-03-08 23:59:59"),]


plot(sub1$time, sub1$light_1, type="l", xlab="", ylab="light_1")
