# plots all spectral scans in the folder


setwd("//Volumes/Extended/OO/timeseries/09feb/")
scans = list.files(pattern = "*.txt") # create list of text files
tot.scans=length(scans) # number of *.txt files in the directory


scan_a = scans[c(TRUE,TRUE,TRUE,FALSE,FALSE,FALSE)]
scan_b = scans[c(FALSE,FALSE,FALSE,TRUE,TRUE,TRUE)]

#n=4
for (n in 1:length(scan_a)){

  file_n = scan_a[n] # change this to the list of files = list.files(pattern = "*.txt")[n]
  data_frame2 = readLines(as.character(file_n))[3]
  data_frame3 = substr(as.character(data_frame2), 18, nchar(as.character(data_frame2))-9)  # truncate string
  data_frame = read.table(as.character(file_n), skip=14, header=F, sep = "\t", col.names = c("wavelength", "intensity"))
  attach(data_frame)

  # o2b = data_frame[5,2] # extracts single waveband
  # o2b = lapply(o2b[2], mean)


  o2b = subset(data_frame, wavelength > 686.7)
  o2b = subset(o2b, wavelength < 688.3)
  o2b.mod = lm(o2b$wavelength ~ o2b$intensity)
  o2b.mod$coefficients[2] -> o2bslope

  o2a = subset(data_frame, wavelength > 759.3)
  o2a = subset(o2a, wavelength < 762.0)
  o2a.mod = lm(o2a$wavelength ~ o2a$intensity)
  o2a.mod$coefficients[2] -> o2aslope


  df = data.frame(data_frame3, o2bslope, o2aslope)
  write.table(df, file="scan1.csv", eol = "\r", na = "NA", row.names = FALSE, append = TRUE, sep=",", col.names = !file.exists("summary.csv"))

}


# merging dataframes https://docs.tibco.com/pub/enterprise-runtime-for-R/2.5.0/doc/html/base/merge.html
#
# Renaming column heads http://www.cookbook-r.com/Manipulating_data/Renaming_columns_in_a_data_frame/
# data import: https://cran.r-project.org/doc/manuals/r-devel/R-data.html
