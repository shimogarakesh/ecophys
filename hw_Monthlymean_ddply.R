################################################################################################################################################################
# creating dataframe with monthly aggregates: one extra year before the time period of the data added 
# variables mapped to one year before the actual period it represents. 
# e.g., 2001 Jan-feb was considered as part of 2001 summer starting from December 2000  

library(plyr)
library(grid)
library(gridExtra)

# assigns a dataframe-vector
names <- vector('list',1) # understand vector definition
names[[1]] <- 'ARAGARCAS_GO_83368'
data <- vector('list',1)
data[[1]] <- read.csv('ARAGARCAS_GO_83368m.csv')
data_frame <- data[[1]]

# Year bounds
yrmax <- max(data_frame[,4])
yrmin <- min(data_frame[,4])
yeartot <- yrmax - yrmin + 1
yrlist <- c(yrmin:yrmax)

summary(data_frame[,4])

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

# ammual aggergates
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

sum_prcp_jf <- ddply(jf, .(year), summarise, sum_prcp_jf <- mean(prcp, na.rm = TRUE))
sum_prcp_dec <- ddply(dec, .(year), summarise, sum_prcp_dec <- mean(prcp, na.rm = TRUE)) # this can be modified to calculate total summer rainfall

summer_year <- c((yrmin-1), (sum_prcp_jf[,1]))
tmax_jf <- c((sum_tmax_jf[,2]),NA)
tmax_d <- c( NA, (sum_tmax_dec[,2]), NA)
tmin_jf <- c((sum_tmin_jf[,2]),NA)
tmin_d <- c( NA, (sum_tmin_dec[,2]), NA)
prcp_jf <- c((sum_prcp_jf[,2]),NA)
prcp_d <- c( NA, (sum_prcp_dec[,2]), NA)

# Aggergating all summer aggregate columns
df <- data.frame("Year" = summer_year, "tmax_jf" = tmax_jf, "tmax_d" = tmax_d, "tmin_jf" = tmin_jf,"tmin_d" = tmin_d, "prcp_jf"=prcp_jf, "prcp_d"=prcp_d)  

summary(df) 

df[,8] <- ((df[,2]+df[,3])/2)
df[,9] <- ((df[,4]+df[,5])/2)
df[,10] <- ((df[,6]+df[,7])/2)

df <- df[ -c(2:7) ]

names(df)[2] <- "tmax_djf_mean"
names(df)[3] <- "tmin_djf_mean"
names(df)[4] <- "prcp_djf_mean"
tminmin <- min(df$tmin_djf_mean, na.rm = T)
tmaxmax <- max(df$tmax_djf_mean, na.rm = T)


tmax_anom <- df$tmax_djf_mean - mean(df$tmax_djf_mean, na.rm = TRUE)
tmin_anom <- df$tmin_djf_mean - mean(df$tmin_djf_mean, na.rm = TRUE)
prcp_anom <- df$prcp_djf_mean - mean(df$prcp_djf_mean, na.rm = TRUE)

df <- data.frame(df, "tmax_anom"=tmax_anom, "tmin_anom"=tmin_anom, "prcp_anom"=prcp_anom)  


# line plot tmax annual means
plot(y=df[,2], x=df[,1], type = "o", col="blue", xlab = "", xlim = c(yrmin,yrmax+4), ylab = expression("Maximum temperature"~degree~C))
plot(y=df[,3], x=df[,1], type = "o", col="blue", xlab = "", xlim = c(yrmin,yrmax+4), ylab = expression("Minimum temperature"~degree~C))
plot(y=df[,4], x=df[,1], type = "o", col="blue", xlab = "", xlim = c(yrmin,yrmax+4), ylab = "precipitation mean")

# change anomoly plot


#png(filename = "ARAGARCAS_GO_83368.png", width = 2000, height = 400, res = 250)

png(filename = "ARAGARCAS_GO_83368.png")
plot(tmax_anom, x=df[,1], type = "l", col="red", xlab = "", xlim = c(yrmin,yrmax),ylab = expression("Temperature"~degree~C), ylim=c(-2,2))
lines(tmin_anom, x=df[,1], col="blue")
abline(h=0)
leg_txt <- c("maximum", "minimum")
col_code <- c("red", "blue")
legend("topleft", legend = leg_txt, fill=col_code, horiz = TRUE, cex = 1)

dev.off()

#write.csv(df, file="aggregate.csv", eol = "\r", na = "NA", row.names = FALSE)

