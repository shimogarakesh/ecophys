# create a list of files in a folder directly in R.
# list.files allows creation of bulk file list. Mac Terminal command cannot handle large volumes of files
# Savitzky Golay filtering requires "prospectr" libraries
# extract intensity column from scan files and row binds them together into a single dataset - the output needs to be transposed to get time as colum. Time data should be appeneded; source: https://psychwire.wordpress.com/2011/06/03/merge-all-files-in-a-directory-using-r-into-a-single-dataframe/
# parse date; http://stackoverflow.com/questions/25463523/convert-variable-with-mixed-date-formats-to-one-format-in-r
# extract a line from the text file http://stackoverflow.com/questions/14261776/extracting-data-from-text-files
# skipping through for loop items http://stackoverflow.com/questions/19021704/for-loop-in-r-which-skips-some-iterations check answer 2
#

library(prospectr)
library(prospectr)
library(stringr)
library(lubridate)


setwd("/Volumes/Extended/OO/17Jan2017/")
setwd("/Volumes/Extended/OO/14Jan2017/")
setwd("/Volumes/Extended/OO/19Jan2017/")
setwd("/Volumes/Extended/OO/20170420_ana/")
setwd("/Volumes/Extended/OO/20120117/")
setwd("//Volumes/Extended/OO/20170206_sub/")
setwd("/Volumes/Extended/OO/20170322/")


file_list = list.files(pattern = "*.txt")


for (file in file_list){
  if (!exists("dataset")){
    dataset <- (read.table(file, skip=14, header=FALSE, sep="\t", col.names = c("wavelength", "intensity")))[2]
  }
  if (exists("dataset")){
    temp_dataset <-(read.table(file, skip=14, header=FALSE, sep="\t", col.names = c("wavelength", "intensity")))[2]
    dataset<-cbind(dataset, temp_dataset)
    rm(temp_dataset)
  }

}

for (file in file_list){
  if (!exists("time_stamp")){
    time_stamp = substr(as.character(read.table(file, skip=2, header=FALSE, sep="\t")[1,1]), 11, 40)
  }
  if (exists("time_stamp")) {
    temp_time_stamp = substr(as.character(read.table(file, skip=2, header=FALSE, sep="\t")[1,1]), 11, 40)
    time_stamp = rbind(time_stamp, temp_time_stamp)
    rm(temp_time_stamp)
  }

}

dataset = as.data.frame(t(dataset)) # transpose combined dataframe
wavelength = as.data.frame(read.table("AbsoluteIrradiance_00-14-49-976.txt", skip=14, header=FALSE, sep="\t")[,1])
wavelength = as.data.frame(t(wavelength))
time_stamp = as.data.frame(as.table(time_stamp))
time_stamp = as.data.frame(time_stamp[,3])
dataset = cbind(time_stamp, dataset)
dataset[,1] = parse_date_time(dataset[,1], orders = c("m d h m s Y"))
colnames(dataset) = c("time", wavelength)

dim(dataset)

plot(dataset$time, dataset[,564])

library(lattice)

levelplot(Height ~ x*y, data = elevation.fit,
          xlab = "X Coordinate (feet)", ylab = "Y Coordinate (feet)",
          main = "Surface elevation data",
          col.regions = terrain.colors(100)
)


# write to output file to a file
write.table(dataset, "/Volumes/Extended/OO/combined_22_march_2017", sep="\t")



















