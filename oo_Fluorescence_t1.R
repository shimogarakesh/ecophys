# To plot all the scans from spectrometer as simple line graphs
# setwd("//Volumes/Extended/OneDrive - University of Leeds/Oceanoptics/scans")
setwd("//Volumes/Extended/OO_20161207/")


filenames = read.table('filelist', header = FALSE)

nsites=dim(filenames)[1]

for (n in 1:nsites){
# n=45
  file_n = filenames[n,]
  tstr <- substr(as.character(file_n), 1, nchar(as.character(file_n))-4)

  d1 = read.table(as.character(file_n),skip=14, header=F, sep = "\t", col.names = c("wavelength", "intensity"))
  quartz()
  png(filename = paste(tstr,".png"), width = 1200, height= 800, res = 150)
  plot(d1$wavelength, d1$intensity,
       xlab = "wavelength (nm)",
       ylab = "Intensity counts",
       type = "l", pch= 19, col=rgb(0,0,1,alpha=1), cex= 0.8, frame.plot = TRUE)
  text(680,(max(d1$intensity)), tstr, cex=0.8, col="grey50", adj = 0)
  dev.off()
  graphics.off()

}


