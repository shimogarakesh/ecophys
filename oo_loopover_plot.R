# create a list of files in a folder directly in R.
# list.files allows creation of bulk file list. Mac Terminal command cannot handle large volumes of files
# Savitzky Golay filtering requires "prospectr" libraries
install.packages("prospectr")
library(prospectr)
setwd("/Volumes/Extended/OO/17Jan2017/")
setwd("/Volumes/Extended/OO/14Jan2017/")
setwd("/Volumes/Extended/OO/19Jan2017/")
setwd("/Volumes/Extended/OO/20170420_ana/")
setwd("/Volumes/Extended/OO/20120117/")



# trial bit to be deleted
# 2017_Jan_Subt11_10-20-51-789
#data_frame = read.table("2017_Jan_Subt11_10-20-51-789.txt", skip=14, header=F, sep = "\t", col.names = c("wavelength", "intensity"))
#scan.name= "trial"
# the two lines below sorts alernate scans in a single list into sun and plant scan.
# scan_plant = scans[c(TRUE, FALSE)]
# scan_sun = scans[c(FALSE, TRUE)]
#scan_odd = scans[c(TRUE, FALSE)]
#scan_even = scans[c(FALSE, TRUE)]
# tot.scans=length(list.files(pattern = "*.txt")) # number of *.txt files in the directory

filenames = list.files(pattern = "*.txt")
# creates a list of all .txt in the working directory
scans = list.files(pattern = "*.txt") # creates a list of all .txt in the working directory
tot.scans=length(scans) # number of *.txt files in the directory
nsites=length(scans)




for (n in 1:nsites){

  n=360
  file_n = scans[n] # change this to the list of files = list.files(pattern = "*.txt")[n]

  data_frame = read.table(as.character(file_n), skip=14, header=F, sep = "\t", col.names = c("wavelength", "intensity"))
  scan.name = substr(as.character(file_n), 7, nchar(as.character(file_n))-8)
# truncate the file name characters.
# O2A band: 759 to 771 nm - the boundary of far infrared http://dx.doi.org/10.1175/1520-0426(1998)015<1272:AMOAMF>2.0.CO;2
  png(filename = paste(scan.name,".png"), width = 1200, height= 1200, res = 90)
  # quartz()
  layout(matrix(c(1,1,2,3),2,2, byrow = TRUE))
  attach(data_frame)
  plot(wavelength, intensity, main = gsub("-", ":", scan.name), type="l", font.main=1, cex.main=0.8, xlab = NULL, ylab = "Intensity (counts)", xlim = c(678.7, 770), cex.lab=0.8, cex.axis=0.8) # scan.name = gsub("-", ":", scan.name) # replace hyphen with colon in the string

  rect(683, (min(data_frame[,2])-200), 692, (max(data_frame[,2])+1000), col = rgb(0.5,0.5,0.5,0.2))
  abline(v = 687, col="blue")
  text(693, (max(data_frame[,2])), "O2-B (687nm)", cex=0.7, col="blue", adj = 0)
  #abline(h=0, col="grey")

  rect(757, (min(data_frame[,2])-200), 771, (max(data_frame[,2])+1000), col = rgb(0.5,0.5,0.5,0.2))
  abline(v = 761, col="blue")
  text(746, (max(data_frame[,2])), "O2-A (761nm)", cex=0.7, col="blue", adj = 0)

  rect(714, (min(data_frame[,2])-200), 722, (max(data_frame[,2])+1000), col = rgb(0.5,0.5,0.5,0.2))
  #abline(v = 719, col="blue")
  text(722.5, (max(data_frame[,2])), "Water vapour", cex=0.7, col="blue", adj = 0) # peaking at 719nm

  #text(740, 110, scan.name, cex=0.9, col="grey20", adj = 0)
  plot(wavelength, intensity, main = "O2-B (688nm)", type="l", xlim = c(685,697),font.main=1, cex.main=0.8, xlab = "Wavelength (nm)", ylab = "Intensity (counts)", cex.lab=0.8, cex.axis=0.8)
  # abline(v = 686, col="blue")
  # text(689, 100, "O2-B (688nm)", cex=0.9, col="blue", adj = 0)
  # abline(h=0, col="grey")

  plot(wavelength, intensity, main = "O2-A (761nm)", type="l", xlim = c(755,770),ylim=c(min(subset(data_frame,data_frame$wavelength >=755)[,2]),max(subset(data_frame,data_frame$wavelength >=755)[,2])), font.main=1, cex.main=0.8, xlab = "Wavelength (nm)", ylab = "Intensity (counts)", cex.lab=0.8, cex.axis=0.8)


  #abline(v = 760, col="blue")
  #text(765, 100, "O2-A (761nm)", cex=0.9, col="blue", adj = 0)
  #abline(h=0, col="grey")

  # waveband range and peaks based on Zhao 2014 10.3390/rs61010171

  #abline(h=0, col="grey")
  rm(data_frame)
  dev.off()
  graphics.off()
  detach(data_frame)
}

























# for set two, run the same set of code below together


 for (n in 1:tot.scans){

   file_n = scan_even[n] # change this to the list of files = list.files(pattern = "*.txt")[n]

   data_frame = read.table(as.character(file_n), skip=14, header=F, sep = "\t", col.names = c("wavelength", "intensity"))
   scan.name = substr(as.character(file_n), 17, nchar(as.character(file_n))-8) # truncate the file name characters.
   # O2A band: 759 to 771 nm - the boundary of far infrared http://dx.doi.org/10.1175/1520-0426(1998)015<1272:AMOAMF>2.0.CO;2
   png(filename = paste(scan.name,".png"), width = 1200, height= 1200, res = 90)
   layout(matrix(c(1,1,2,3),2,2, byrow = TRUE))
   attach(data_frame)
   plot(wavelength, intensity, main = NULL, type="l", font.main=1, cex.main=0.8, xlab = NULL, ylab = "Intensity (counts)", cex.lab=0.8, cex.axis=0.8)
   rect(683, -200, 692, 600, col = rgb(0.5,0.5,0.5,0.2))
   abline(v = 687, col="blue")
   # text(688, -50, "O2-B (687nm)", cex=0.9, col="blue", adj = 0)
   abline(h=0, col="grey")

   rect(757, -200, 771, 600, col = rgb(0.5,0.5,0.5,0.2))
   abline(v = 761, col="blue")
   text(762, -50, "O2-A (761nm)", cex=0.9, col="blue", adj = 0)

   rect(714, -200, 722, 600, col = rgb(0.5,0.5,0.5,0.2))
   abline(v = 719, col="blue")
   # text(720, -50, "Water vapour (719nm)", cex=0.9, col="blue", adj = 0)

   scan.name = gsub("-", ":", scan.name) # replace hyphen with colon in the string
   text(740, 110, scan.name, cex=0.9, col="grey20", adj = 0)
   plot(wavelength, intensity, main = "O2-B (688nm)", type="l", xlim = c(685,690),font.main=1, cex.main=0.8, xlab = "Wavelength (nm)", ylab = "Intensity (counts)", cex.lab=0.8, cex.axis=0.8)
   abline(v = 686, col="blue")
   #text(689, 100, "O2-B (688nm)", cex=0.9, col="blue", adj = 0)
   abline(h=0, col="grey")

   plot(wavelength, intensity, main = "O2-A (761nm)", type="l", xlim = c(745,770),font.main=1, cex.main=0.8, xlab = "Wavelength (nm)", ylab = "Intensity (counts)", cex.lab=0.8, cex.axis=0.8)
   abline(v = 760, col="blue")
   #text(765, 100, "O2-A (761nm)", cex=0.9, col="blue", adj = 0)
   abline(h=0, col="grey")

   # waveband range and peaks based on Zhao 2014 10.3390/rs61010171

   abline(h=0, col="grey")
   rm(data_frame)
   dev.off()
   graphics.off()
   detach(data_frame)
 }









# write summary statistics of the scans into a csv file.
# A check to see if the files are different

for (n in 1:tot.scans){

  file_n = list.files(pattern = "*.txt")[n]

  data_frame = read.table(as.character(file_n),
                          skip=14, header=F,
                          sep = "\t",
                          col.names = c("wavelength", "intensity"))
  scan.name <- substr(as.character(file_n), 23, nchar(as.character(file_n))-4)
  sig.mean = round(mean(data_frame$intensity), 2)
  sig.sd = round(sd(data_frame$intensity), 2)
  sig.med = round(median(data_frame$intensity),2)
  df = data.frame(scan.name, sig.mean, sig.med, sig.sd)
  write.table(df, file="summary.csv", eol = "\r", na = "NA",
              row.names = FALSE, append = TRUE,
              sep=",", col.names = !file.exists("summary.csv"))

}



summary_out = read.csv("summary.csv", header = TRUE)

attach(summary_out)
quartz()
plot(summary_out$sig.mean, type = "l", ylim=c(0,200))
plot(data_frame$wavelength, data_frame$intensity, type = "l", xlim = c(755,770), ylim = c(0, 350))


plot(data_frame$wavelength, data_frame$intensity, type = "l" )
rect(685, -100, 700, 3500, density = NULL, angle = 45,
     col = rgb(0.5, 0.5, 0, 0.5), border = 0) # O2B
rect(759, -100, 770, 3500, density = NULL, angle = 45,
     col = rgb(0.5, 0.5, 0, 0.5), border = 0) # O2A


# extract time from the scans

library(stringr)


for (n in 1:tot.scans){

  file_n = list.files(pattern = "*.txt")[n]

  data_frame = read.table(as.character(file_n),
                          skip=14, header=F,
                          sep = "\t",
                          col.names = c("wavelength", "intensity"))
  f <- readLines(as.character(file_n))
  scan.name <- substr(as.character(file_n), 23, nchar(as.character(file_n))-4)
  sig.mean = round(mean(data_frame$intensity), 2)
  sig.sd = round(sd(data_frame$intensity), 2)
  sig.med = round(median(data_frame$intensity),2)
  df = data.frame(scan.name, sig.mean, sig.med, sig.sd)
  write.table(df, file="summary.csv", eol = "\r", na = "NA",
              row.names = FALSE, append = TRUE,
              sep=",", col.names = !file.exists("summary.csv"))

}



cline <- grep("Date:",f, value=TRUE)
val <- as.numeric(str_extract(cline,"[0-9]+$"))
strptime(cline, )

# ####################################  misc extra
#
#
#

par(mfrow=c(3,1))
plot((savitzkyGolay(data_frame[,2], m=2, p=10, w=11)), type="l", )
plot((savitzkyGolay(data_frame[,2], m=5, p=5, w=51)), type="l")
plot((savitzkyGolay(data_frame[,2], m=5, p=5, w=91)), type="l")
plot((savitzkyGolay(data_frame[,2], m=5, p=5, w=121)), type="l")

plot((savitzkyGolay(data_frame[,2], m=2, p=70, w=101)), type="l")

#######################
#######################
#######################
data.smth = c(NA, NA, NA, NA, NA,NA, NA, NA, NA, NA,NA, NA, NA, NA, NA,NA, NA, NA, NA, NA,NA, NA, NA, NA, NA,NA, NA, NA, NA, NA,NA, NA, NA, NA, NA,NA, NA, NA, NA, NA,NA, NA, NA, NA, NA, (savitzkyGolay(data_frame[,2], m=3, p=10, w=91)), NA, NA, NA, NA, NA,NA, NA, NA, NA, NA,NA, NA, NA, NA, NA,NA, NA, NA, NA, NA,NA, NA, NA, NA, NA,NA, NA, NA, NA, NA,NA, NA, NA, NA, NA,NA, NA, NA, NA, NA,NA, NA, NA, NA, NA)
ccc = data.frame(data_frame[,1],data.smth)
plot(ccc[,1], ccc[,2], type="l")

