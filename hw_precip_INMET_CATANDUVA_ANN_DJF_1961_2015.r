setwd("/Work/Data_Model_Obs/Obs_Data/INMET_sao_paulo_precip")

names <- vector('list',1)
names[[1]] <- 'ARAGARCAS_GO_83368'

data <- vector('list',1)
data[[1]] <- read.csv('ARAGARCAS_GO_83368.csv')

# column heads: 
# Estacao - station
# Data date: sub divided into day, month and year
# Hora - hour
# Precipitacao - precipitation
# TempMaxima - Tmax
# TempMinima - Tmin
# Insolacao - ??????/
# Evaporacao Piche - ?????
# Temp Comp Media - Average temperature???
# Umidade Relativa Media - mean Rh
# Velocidade do Vento Media - Average wind speed

data_cat <- data[[1]]
DD       <- data_cat[,2]
MM       <- data_cat[,3]
YYYY     <- data_cat[,4]
FRACYR <- ((MM-1)*(365/12)+DD)/365 
YR <- YYYY+FRACYR

yrlist <- c(yrmin:yrmax)

prcp <- data_cat[,6]
tmax <- data_cat[,7]
tmin <- data_cat[,8]



yrmax <- max(data_cat[,4])
yrmin <- min(data_cat[,4])
yeartot <- yrmax - yrmin + 1

II<- which(is.finite(tmax))

ann_tmax <- rep(0,yeartot)
ann_tmax_anom <- rep(0,yeartot)
for (NY in yrmin:yrmax){
   nsamp <- 0
   for (mo in 1:12){
      II <- which((YYYY==NY)&(MM==mo))
      if (length(tmax[II])>0) {
    	 ann_tmax[NY-yrmin+1] <- ann_tmax[NY-yrmin+1] + tmax[II]
    	 nsamp    <- nsamp + 1
      }
   }
   ann_tmax[NY-yrmin+1] <- (1/nsamp)*ann_tmax[NY-yrmin+1]    
  }

ann_tmax_anom <- ann_tmax - mean(ann_tmax, na.rm=T)
yl = expression(paste("(mm yr"^-1,")"))

summary(ann_tmax)
length(ann_tmax)
length(YR)

plot(yrlist, ann_tmax, main = "ARAGARCAS GO")
