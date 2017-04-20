# plots all spectral scans in the folder


setwd("//Volumes/Extended/OO/timeseries/09feb/")
scans = list.files(pattern = "*.txt") # create list of text files
tot.scans=length(scans) # number of *.txt files in the directory


scan_a = scans[c(TRUE,TRUE,TRUE,FALSE,FALSE,FALSE)]
scan_b = scans[c(FALSE,FALSE,FALSE,TRUE,TRUE,TRUE)]


for (n in 1:tot.scans){

  file_n = scans[n] # change this to the list of files = list.files(pattern = "*.txt")[n]
  data_frame2 = readLines(as.character(file_n))[3]
  data_frame3 = substr(as.character(data_frame2), 11, nchar(as.character(data_frame2))-9)  # truncate string
  data_frame = read.table(as.character(file_n), skip=14, header=F, sep = "\t", col.names = c("wavelength", "intensity"))
  data_frame3 = gsub(" ", "_", data_frame3) # replace space with underscore in the string
  data_frame3 = gsub(":", "-", data_frame3) # replace space with underscore in the string

  quartz()
  png(filename = paste(data_frame3,".png"), width = 600, height= 250, res = 80)
  attach(data_frame)
  plot(wavelength, intensity, main = data_frame3, type="l", font.main=1, cex.main=1.2, xlab = NULL, ylab = "Intensity (counts)", cex.lab=0.8, cex.axis=0.8) # scan.name = gsub("-", ":", scan.name) # replace hyphen with colon in the string
  rm(data_frame)
  dev.off()
  graphics.off()
  detach(data_frame)
}

