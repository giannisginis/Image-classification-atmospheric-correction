##atmospheric correction
library(landsat)
library(RStoolbox)
library(devtools)
require(graphics)
library(graphics)
library(satellite)
library(raster)


##_import_stack
metaData<- readMeta(file.choose())
summary(metaData)

##radiometric calibration - atmospheric correction Landsat
lsat     <- stackMeta(metaData)
plotRGB(lsat,r = 3, g = 2, b = 1,stretch="hist")

##crop the image with the specified extend
ext1 <- drawExtent() #draw a box by clicking upper left and lower right corner in the plot
lsat_crop <- crop(lsat, ext1)
plotRGB(lsat_crop,r = 3, g = 2, b = 1,stretch="hist")
writeRaster(lsat_crop, filename="lsat_crop.tif", options="INTERLEAVE=BAND",
            datatype='GTiff', overwrite=TRUE)
##apref method
lsat_ref <- radCor(lsat_crop , metaData = metaData, method = "apref")
writeRaster(lsat_ref, filename="lsat_TOIref.tif", options="INTERLEAVE=BAND",
            datatype='GTiff', overwrite=TRUE)
##dos method         
lsat_ref_dos <- radCor(lsat_crop, metaData = metaData, method = "dos")
writeRaster(lsat_sref, filename="lsat_surfaceref.tif", options="INTERLEAVE=BAND",
            datatype='GTiff', overwrite=TRUE)
##sdos method
plotRGB(lsat,r = 3, g = 2, b = 1,stretch="hist")
hazeDN7 <- estimateHaze(lsat_crop, hazeBands = c(1:5,8),
                          darkProp = 0.01, plot = TRUE)
lsat_ref_sdos <- radCor(lsat_crop, metaData = metaData, method = "sdos", 
                    hazeValues = hazeDN7, hazeBands = c(1:5,8), clamp=TRUE)
writeRaster(lsat_sref, filename="lsat_surfaceref.tif", options="INTERLEAVE=BAND",
            datatype='GTiff', overwrite=TRUE)
