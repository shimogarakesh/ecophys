# plot surface
library(plot3D)
setwd("/Volumes/Extended/OO/")
data_frame = read.table("combined_22_march_2017", sep = "\t", header = TRUE)



image2D(tdf)
filled.contour(tdf)

trialdf = data_frame[1:500,2:2049]
tdf = as.matrix(trialdf)

quartz()
plot()


dim(tdf)


dim(trialdf)


require(rgl)
#  open renderer
open3d()
#  plot surface
rgl.surface( 1:10 , 1:10 , runif(100))

rm(wavelength)

rm(time_stamp)

dataset[,2:2049]

y <- dataset        # Exaggerate the relief

x <- 10 * (1:nrow(y))   # 10 meter spacing (S to N)
z <- 10 * (1:ncol(y))   # 10 meter spacing (E to W)

ylim <- range(y)
ylen <- ylim[2] - ylim[1] + 1

colorlut <- terrain.colors(ylen) # height color lookup table

col <- colorlut[ y - ylim[1] + 1 ] # assign colors to heights for each point

rgl.open()
rgl.surface(x, z, y, color = col, back = "lines")




install.packages("OceanView")
install.packages("plot3Drgl")
library(OceanView)
Long <- WSnioz[as.character(WSnioz$VariableName) == "WNO3",
               c("SamplingDateTimeREAL", "Station", "DataValue")]
Long$year <- Long$SamplingDateTimeREAL / 365 + 1900

# cross table; samples taken within 5 days are assumed to be = campaign
Cross <- db2cross(Long, row = "year", df.row = 5/365,
                  col = "Station", val = "DataValue")

# image plot; increase resolution
image2D(x = Cross$x, y = Cross$y, z = Cross$z,
        resfac = 3, ylim = c(0, 17), log = "",
        main = "Nitrate", xlab = "year", ylab = "station", clab = "mmol/m3")



