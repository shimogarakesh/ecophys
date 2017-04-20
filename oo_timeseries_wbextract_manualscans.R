# plots all spectral scans in the folder


setwd("//Volumes/Extended/OO/")
scans = list.files(pattern = "*.txt") # create list of text files
tot.scans=length(scans) # number of *.txt files in the directory


#scan_a = scans[c(TRUE,TRUE,TRUE,FALSE,FALSE,FALSE)]
#scan_b = scans[c(FALSE,FALSE,FALSE,TRUE,TRUE,TRUE)]


sun = read.table("Subt1_14-03-44-401_sun2500ms_good.txt", skip=14, header=F, sep = "\t", col.names = c("wavelength", "intensity"))
attach(sun)

plant = read.table("Subt1_14-05-09-394_plant2500ms_good.txt", skip=14, header=F, sep = "\t", col.names = c("wavelength", "intensity"))
attach(plant)

dim(plant)
dim(sun)



# L = plant
# E = solar

o2b_sun = subset(sun, wavelength > 675)
o2b_sun = subset(sun, wavelength < 697)
o2a_sun = subset(sun, wavelength > 750)

o2b_plant = subset(plant, plant$wavelength > 675)
o2b_plant = subset(plant, plant$wavelength < 697)
o2a_plant = subset(plant, plant$wavelength > 750)

dim(o2b_sun)
dim(o2b_plant)
o2a = merge(o2a_sun, o2a_plant, by = "wavelength")

names(o2a)[2] = "o2a_sun"
names(o2a)[3] = "o2a_plant"

o2a["plnt_pi"] = NA
o2a[4] = o2a[3]*pi

o2a["upwelling"] = NA
o2a[5] = o2a[4]/o2a[2]



o2b = merge(o2b_sun, o2b_plant, by = "wavelength")

names(o2b)[2] = "o2b_sun"
names(o2b)[3] = "o2b_plant"

o2b["plnt_pi"] = NA
o2b[4] = o2b[3]*pi

o2b["upwelling"] = NA
o2b[5] = o2b[4]/o2b[2]




plot(o2a$wavelength, o2a$o2a_sun, type="l")
lines(o2a$wavelength, o2a$o2a_plant, type="l")
lines(o2a$wavelength, o2a$upwelling, type="l")

plot(o2b[,2], type="l")
lines(o2b[,3], type="l")

graphics.off()
par(mar = c(5,5,2,5))
with(o2a, plot(o2a$wavelength, o2a$o2a_sun, type="l", col="red3", ylab="wavelength"))
par(new = T)
with(o2a, plot(o2a$wavelength, o2a$upwelling, type="l", col="blue", ylab="wavelength"), ylim = c(-4,2))
axis(side = 4)

# https://www.r-bloggers.com/r-single-plot-with-two-different-y-axes/


with(d, plot(x, n, pch=16, axes=F, xlab=NA, ylab=NA, cex=1.2))
     axis(side = 4)
     mtext(side = 4, line = 3, 'Number genes selected')



     legend("topleft",
            legend=c(expression(-log[10](italic(p))), "N genes"),
            lty=c(1,0), pch=c(NA, 16), col=c("red3", "black"))







layout(matrix(c(1,2,3,4),2,2, byrow = TRUE))
graphics.off()



summary(o2a)
data_frame = data.frame(o2a_sun)
data_frame[2] = "O2a_sun"
plot(((o2a_plant[,2]*pi)/o2a_sun[,2]), col="black", type="l")

plot(o2a[,1], o2a[,5], type="l", xlab = "Wavelength (nm)", ylab = "Intensity (counts)", main = "O2A", cex.main=0.8)
lines(o2a[,5])
text(7500, 1.6, "O2A", cex=0.9, col="blue") #, adj = 0)




pi_l = (o2a_plant$intensity*pi)
ba = ((o2a_plant$intensity*pi)/o2a_sun$intensity))

dim(pi_l)







o2b.mod = lm(o2b$wavelength ~ o2b$intensity)
o2b.mod$coefficients[2] -> o2bslope

o2a = subset(data_frame, wavelength > 759.3)
o2a = subset(o2a, wavelength < 762.0)
o2a.mod = lm(o2a$wavelength ~ o2a$intensity)
o2a.mod$coefficients[2] -> o2aslope



####################











#n=4
for (n in 1:length(scans)){

  file_n = scans[n] # change this to the list of files = list.files(pattern = "*.txt")[n]
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
  write.table(df, file="scan1.csv", eol = "\r", na = "NA", row.names = FALSE, append = TRUE, sep=",", col.names = !file.exists("scan1.csv"))

}

