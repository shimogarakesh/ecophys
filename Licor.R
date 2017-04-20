# temperature response curves from CSV
# requires XLConnect package
#
setwd("//Volumes/Extended/Licor/")
library(XLConnect)
library(xlsx)
library(XLConnect)
library(XLConnectJars)
library(rJava)
getwd()
df = read.csv("01-ficus-tresp-feb8_.csv", skip = 8)
df = readWorksheetFromFile("01-ficus-tresp-feb8_.xls", sheetIndex = 1)
detach(df)
df = df[-66,]
quartz()
par(mfrow=c(2,2))
attach(df)

plot(Tair,Photo, main="A:Tl", type = "p")
plot(lowess(Tair, Photo), type = "l", ylim = c(min(Photo), max(Photo)), ylab = "Photo", xlab = "Tair")

max(Photo)

plot(Tair,Photo)
hist(wt, main="Histogram of wt")


graphics.off()
