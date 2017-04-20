setwd('/Volumes/Extended/INMET/looping_trial')
setwd('/Volumes/Extended/OO_14Jan2017/')

filenames = list.files(pattern = "*.txt") # creates a list of all .txt in the working directory

nsites=dim(filenames)[1]

for (n in 1:nsites){

  file_n = filenames[n,]

  data_frame = read.csv(as.character(file_n))
  tstr <- substr(as.character(file_n), 1, nchar(as.character(file_n))-10)
  png(filename = paste(tstr,".eps"))
  plot(seq(1,50),sub = tstr)
  abline(h=0)
  leg_txt <- c("maximum", "minimum")
  col_code <- c("red", "blue")
  legend("topleft", legend = leg_txt, fill=col_code, horiz = TRUE, cex = 1)
  dev.off()
}

