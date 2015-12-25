# Author: Sparkle L. Malone
# Date: August 2015
# Objective: Spatial matterns in low temperature events (Only uses days that are freeze events at all three sites)
rm(list=ls())

library(raster)
library(rgdal)
library(prism)
library(splitstackshape)
library(maptools)
# Import data:

setwd("~/git/Cold-Events/ENP")

enp <- readOGR(".", "Everglades_NP")
enp <- spTransform(enp, CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0 "))

# Mean annual Temperature Data 2000- 2013:
options(prism.path = "~/git/Cold-Events/PRISM")
t.min <- ls_prism_data(absPath = TRUE, name=TRUE) # the raster function needs the absolute path
t.min.list <- data.frame(ls_prism_data(absPath = TRUE, name=TRUE)[,2])
t.min.list <- concat.split(t.min.list, 1, sep="_", drop=T)
t.min.list <- data.frame(t.min.list[,5])


names(t.min.list) <- c('a')
t.min.list$b <-t.min.list$a/10000
t.min.list <- concat.split(t.min.list, 2, sep=".", drop=T)
t.min.list$c <- t.min.list$b_2/100
t.min.list <- concat.split(t.min.list, 4, sep=".", drop=T)

t.min.list <- t.min.list[, c(2, 4, 5)]; names(t.min.list) <- c("year", "month", "day")
t.min.list[,2] <-sprintf( "%02d", as.numeric(t.min.list[, 2]))
t.min.list$date <- paste(t.min.list$year, t.min.list$month, t.min.list$day, sep='/' )
t.min.list$date <- as.Date(t.min.list$date, format="%Y/%m/%d" )


t.min <- stack(t.min[,2]) # Create Raster Stack
names(t.min) <- t.min.list$date

t <- calc(t.min, fun=mean)# calculate mean of freeze events.

t <- crop(t, enp)
t <- mask(t, enp)

# Spatial patterns in Raster objects:

library(rasterVis)
library(RColorBrewer)
library(colorRamps)

display.brewer.all()
b <- blue2red(25)

levelplot(t , zscaleLog = F, contour = TRUE, col.regions = b,lwd=2, xlab= expression(paste(" Temperature("~degree,"C)")), ylab= "",
          par.settings=list(cex=2))

# Shows mean min temperature for (1/11/10, 12/14/10, 12/15/10, 12/28/10)
          



