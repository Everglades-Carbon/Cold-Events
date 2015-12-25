### MUST BE RUN FROM VIRTUAL MACHIINE###

# Install packages:
# install.packages(c("MODISTools", "RCurl", "bitops", "rgdal"))

rm(list=ls())

setwd("c:/Modis")

# loading the source of function (the script file should be copied in the working directory):
source('ModisDownload.R')

library(raster)
library(RCurl)
library(bitops)

# product list:

modisProducts( )

# NDVI
x="MOD13A3" #change between Q1 and Q2
test<-.modisHTTP (x,v='005') 


ModisDownload(x=x ,h=10, v=6,dates=c('2009.01.01','2013.12.31'),
              MRTpath='c:/Modis/bin', mosaic=F,proj=T, proj_type="UTM",
              utm_zone=17, datum="WGS84",pixel_size=1000, delete=T)


# Move NDVI files to the proper directory:

setwd('Z:/Promise Pegasus/Data/Everglades_NDVI_FRZ')

# dir.create('Z:/Promise Pegasus/Data/Everglades_NDVI_FRZ')
origindir <- c('c:/Modis')
targetdir <- c('Z:/Promise Pegasus/Data/Everglades_NDVI_FRZ')

mod.ndvi.list  <- list.files(path = "c:/Modis", pattern=".tif")

lapply(mod.ndvi.list, function(x) file.copy(paste (origindir, x , sep = "/"),  
                                            paste (targetdir,x, sep = "/"), recursive = FALSE,  copy.mode = TRUE))

# Takes file of interest and moves them:

setwd("\\\\vmware-host/Shared Folders/Desktop/NDVI")

origindir <- c('Z:/Promise Pegasus/Data/Everglades_NDVI_FRZ')
targetdir <- c("\\\\vmware-host/Shared Folders/Desktop/NDVI")

ndvi.list  <- list.files(path = 'Z:/Promise Pegasus/Data/Everglades_NDVI_FRZ', pattern="NDVI.tif$")


lapply(ndvi.list, function(x) file.copy(paste (origindir, x , sep = "/"),  
                                            paste (targetdir,x, sep = "/"), recursive = FALSE,  copy.mode = TRUE))
# transfer quality flags:

#dir.create("\\\\vmware-host/Shared Folders/Desktop/NDVI/flags")

origindir <- c('Z:/Promise Pegasus/Data/Everglades_NDVI_FRZ')
targetdir <- c("\\\\vmware-host/Shared Folders/Desktop/NDVI/flags")

flags.list  <- list.files(path = 'Z:/Promise Pegasus/Data/Everglades_NDVI_FRZ', pattern="Quality.tif$")


lapply(flags.list, function(x) file.copy(paste (origindir, x , sep = "/"),  
                                        paste (targetdir,x, sep = "/"), recursive = FALSE,  copy.mode = TRUE))



