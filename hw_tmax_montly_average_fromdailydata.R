setwd("/Work/Data_Model_Obs/Obs_Data/INMET_sao_paulo_precip")

names <- vector('list',1)
names[[1]] <- 'ARAGARCAS_GO_83368'

data <- vector('list',1)
data[[1]] <- read.csv('ARAGARCAS_GO_83368.csv', stringsAsFactors = FALSE)

data_cat <- data[[1]]
DD       <- data_cat[,2]
MM       <- data_cat[,3]
YYYY     <- data_cat[,4]
FRACYR <- ((MM-1)*(365/12)+DD)/365 
YR <- YYYY+FRACYR
yrmax <- max(data_cat[,4])
yrmin <- min(data_cat[,4])
yeartot <- yrmax - yrmin + 1
yrlist <- c(yrmin:yrmax)

data_cat[,2] <- as.POSIXct(data_cat[,2], format = "%d/%m/%Y")

data_cat[,2] 


names(data_cat)[1] <- "station"
names(data_cat)[2] <- "date"
names(data_cat)[3] <- "hour"
names(data_cat)[4] <- "prcp"
names(data_cat)[5] <- "tmax"
names(data_cat)[6] <- "tmin"



# library(data.table)
# out <- setDT(data_cat)[, .(MontlyMeans = (mean(tmax), na.rm = TRUE)), by = .(year(date), month(date))]

#plot(out$year, out$MontlyMeans)




library(dplyr)
library(lubridate)

data_cat$date = ymd(data_cat$date)

data_cat %>% 
  group_by(Year=year(date), Month=month(date)) %>%
  summarise(meanDailyMax=(mean(data_cat$tmax),
            meanDailyMinn=mean(data_cat$tmin))

Year Month meanDailyMin meanDailyMean
1 1949     1       11.095      11.71928









###############






library(dplyr)
library(lubridate)

data_cat$date = ymd(data_cat$date)

data_cat %>% 
  group_by(Year=year(date), Month=month(date)) %>%
  summarise(meanDailyMax=mean(tmax),
            meanDailyMin=mean(tmin))


??summarise()

Year Month meanDailyMin meanDailyMean
1 1949     1       11.095      11.71928
head(data_cat)
prcp <- as.numeric(data_cat[,6], na.rm=TRUE)
tmax <- as.numeric(data_cat[,7], na.rm=TRUE)
tmin <- as.numeric(data_cat[,8], na.rm=TRUE)
library(data.table)

formatted <- data.frame(data_cat[,4],data_cat[,5])

summary(data_cat[,7])
summary(data_cat)

str(data_cat)
setDT(data_cat)[, .(MontlyMeans = mean(data_cat[,7])), by = .(year(data_cat[,4]), month(data_cat[,3]))]
#    year month MontlyMeans
# 1: 1949     1    11.71928


setDT
II<- which(is.finite(tmax))

ann_tmax <- rep(0,yeartot)
ann_tmax_anom <- rep(0,yeartot)
for (NY in yrmin:yrmax){
  nsamp <- 0
  for (mo in 1:12){ #all months
    II <- which((YYYY==NY)&(MM==mo))
    if (length(tmax[II])>0) {
      ann_tmax[NY-yrmin+1] <- ann_tmax[NY-yrmin+1] + tmax[II]
      nsamp    <- nsamp + 1
    }
  }
  ann_tmax[NY-yrmin+1] <- (1/nsamp)*ann_tmax[NY-yrmin+1]    
}
ann_tmax_anom <- ann_tmax - mean(ann_tmax, na.rm=T)


summary(data_cat[,7])



towrite <- cbind(yrlist, ann_tmax)


plot(towrite[,1], towrite[,2])

write.csv(towrite, file="text.csv", eol = "\r", na = "NA", row.names = FALSE)
??write.csv()

read.csv(text.csv)
summary(ann_tmax)
length(ann_tmax)
length(YR)

plot(x=yrlist, y=ann_tmax, sub = "ARAGARCAS GO",  ylab = "tmax", xlab = "")


summary(ann_tmin_anom)




#Box Plot for both variables

boxplot(ann_tmax_anom, ann_tmax_anom main="Boxplot",  ylab ="temperature", xlab ="",las= 2, cex.axis=0.7, col = c("gery","lightgreen"), at = c(1,2), names = c("tmax","tmin"))
dev.off()





grid.arrange(plotmax, plotmin,ncol=2)
#png(filename = "82596_lus_mn_hist.png", width = 2000, height = 2400, res = 250)
grid.arrange(l2pn,l3pn, l4pn, l5pn, l6pn, l7pn, l8pn, l9pn, l10pn, l11pn, l12pn, l13pn, ncol=2)
#dev.off()




# column heads: # Estacao - station # Data date: sub divided into day, month and year
# Hora - hour # Precipitacao - precipitation # TempMaxima - Tmax # TempMinima - Tmin
# Insolacao - ??????/ # Evaporacao Piche - ?????
# Temp Comp Media - Average temperature??? # Umidade Relativa Media - mean Rh
# Velocidade do Vento Media - Average wind speed
