# MODIS NDVI ANALYSIS:

rm(list=ls())

library(raster)
library(rgdal)
library(splitstackshape)

# Import shapefiles
setwd("~/git/Cold-Events/ENP")

sites <- readOGR(".", "Freeze_sites")
sites <- spTransform(sites, CRS("+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# Import raster stacks of NDVI and Quatity Flags:
setwd("~/git/Cold-Events/NDVI") # Set the working directory.

ndvi.list  <- list.files(path = '~/git/Cold-Events/NDVI', pattern="NDVI.tif$")
flags.list  <- list.files(path = '~/git/Cold-Events/NDVI', pattern="Quality.tif$")

# # Adjust based on filters (https://lpdaac.usgs.gov/dataset_discovery/modis/modis_products_table/mod13a3):
ndvi <-stack(ndvi.list) 
ndvi[ndvi == -3000]<- NA
ndvi <- ndvi* 0.0001

flags <-stack(flags.list)


# add an index to the raster stack:
ndvi.idx <- as.data.frame(ndvi.list) # convert list to dataframe
ndvi.idx <-concat.split(ndvi.idx, 1, sep="_", drop=T);ndvi.idx <-data.frame(ndvi.idx[,3]) # split and select column of interest
ndvi.idx <-concat.split(ndvi.idx, 1, sep="-", drop=T); ndvi.idx <-data.frame(ndvi.idx[ ,1:2])# split and select column of interest
names(ndvi.idx) <-c("year", "month") # renames columns
ndvi.idx[,2] <-sprintf( "%02d", as.numeric(ndvi.idx[, 2])) # adds a 0 in front of single digits for the month
ndvi.idx$date <-paste(ndvi.idx$year, ndvi.idx$month, sep="-") # combines the month and year

flags.idx <- as.data.frame(flags.list) # convert list to dataframe
flags.idx <-concat.split(flags.idx, 1, sep="_", drop=T);flags.idx <-data.frame(flags.idx[,3]) # split and select column of interest
flags.idx <-concat.split(flags.idx, 1, sep="-", drop=T); flags.idx <-data.frame(flags.idx[ ,1:2])# split and select column of interest
names(flags.idx) <-c("year", "month") # renames columns
flags.idx[,2] <-sprintf( "%02d", as.numeric(flags.idx[, 2])) # adds a 0 in front of single digits for the month
flags.idx$date <-paste(flags.idx$year, flags.idx$month, sep="-") # combines the month and year

library(zoo)
ndvi.index <- as.Date(as.yearmon(ndvi.idx$date)) # creates an index in date format
flags.index <- as.Date(as.yearmon(flags.idx$date))

names(ndvi) <- ndvi.index # changes names to match index
names(flags) <- flags.index

library(rts)
ndvi <- rts(ndvi,ndvi.index) # adds the index to the raster stack creating a raster time series. 
flags <- rts(flags,flags.index)


sites.df <- data.frame(extract(ndvi, sites))
names(sites.df) <- c("srs2", "ts1", "srs6")
sites.df$date<- as.Date(row.names(sites.df), "%Y-%m-%d") # Format the date column

#___________________________________________FIGURE:NDVI_________________________________________________
par(mfrow=c(3,1), mai=c(1, 1.5, 0.25, 0.25), tck=0.02, cex=1.1)
plot(sites.df$date, sites.df$srs2, ylim=c(0.40, 1), xlab="", ylab=" SRS-2  NDVI")
abline(v=as.Date('2009-01-01'), lty=2)
abline(v=as.Date('2009-02-01'), lty=2)
abline(v=as.Date('2010-01-01'), lty=2)
abline(v=as.Date('2010-02-01'), lty=2)
abline(v=as.Date('2010-03-01'), lty=2)
abline(v=as.Date('2010-12-01'), lty=2)
abline(v=as.Date('2011-01-01'), lty=2)
abline(v=as.Date('2012-01-01'), lty=2)
mtext(text="a.", side=3, adj=0.01, cex=1.1, outer=F, line=-1)
plot(sites.df$date, sites.df$ts1, col="dimgrey",ylim=c(0.40, 1), xlab="", ylab=" TS-1  NDVI")
abline(v=as.Date('2009-01-01'), lty=2)
abline(v=as.Date('2009-02-01'), lty=2)
abline(v=as.Date('2009-03-01'), lty=2)
abline(v=as.Date('2010-01-01'), lty=2)
abline(v=as.Date('2010-02-01'), lty=2)
abline(v=as.Date('2010-03-01'), lty=2)
abline(v=as.Date('2010-12-01'), lty=2)
abline(v=as.Date('2011-01-01'), lty=2)
abline(v=as.Date('2011-03-01'), lty=2)
abline(v=as.Date('2012-01-01'), lty=2)
abline(v=as.Date('2012-03-01'), lty=2)
mtext(text="b.", side=3, adj=0.01, cex=1.1, outer=F, line=-1)
plot(sites.df$date, sites.df$srs6, col="blue",ylim=c(0.40, 1), xlab="", ylab=" SRS-6  NDVI")
abline(v=as.Date('2010-01-01'), lty=2)
abline(v=as.Date('2010-12-01'), lty=2)
abline(v=as.Date('2012-01-01'), lty=2)
mtext(text="c.", side=3, adj=0.01, cex=1.1, outer=F, line=-1)
#___________________________________________FIGURE:NDVI_________________________________________________

# Need to run CE-2 and CE-3a prior to this section!!!

# Extract raster values for each site (Time Series): Convert to a dataframe.
sites.df$yearmon <- format(sites.df$date, "%Y-%m")

Frz.dates$date <- as.Date(Frz.dates$date, format= "%Y-%m-%d")
Frz.dates$yearmon <- format(Frz.dates$date, "%Y-%m")
Frz.dates[ is.na(Frz.dates)]<- 0

events <- aggregate(Frz.dates[ ,2:4], by= list(Frz.dates$yearmon), FUN = max)

names(events) <- c('yearmon', 'ts.events', 'srs.events', 'ms.events')

events[ is.na(events)]<- 0

ndvi.df <- merge(sites.df, events, by='yearmon', all=T)
ndvi.df<- ndvi.df[which(ndvi.df$srs2 > 0),]
ndvi.df[ is.na(ndvi.df)]<- 0

#create ts objects
srs.ndvi <- ts( ndvi.df$srs2 , start=c(2009, 01), end=c(2012, 12), frequency=12 )


# test for normality
ks.test(ndvi.df$srs2,"pnorm",mean(ndvi.df$srs2),sd(ndvi.df$srs2)) # significant p-value= non-normal
shapiro.test(srs.ndvi)

test<- lag(srs.ndvi,k=-2)

# Test autocorrelation:
library(lmtest)
dwtest(srs.ndvi ~ lag(srs.ndvi,k=-1)) # Significant p-value suggest autocorrelation

# Create an indicator for each site for cold events. Look for changes in greeness following cold events:
ndvi.df$date <- as.Date(as.yearmon(ndvi.df$yearmon))
ndvi.df$year <- format(ndvi.df$date, "%Y")
ndvi.df$month<- format(ndvi.df$date, "%m")

ndvi.frz.ts1 <- ndvi.df[which(ndvi.df$ts.events == 1),]
ndvi.frz.ts1 <- aggregate(ndvi.frz.ts1$ts1, by= list(ndvi.frz.ts1$month), FUN= mean); names(ndvi.frz.ts1 ) <- c("month", "ts.frz")

ndvi.frz.srs <- ndvi.df[which(ndvi.df$srs.events == 1),]
ndvi.frz.srs <- aggregate(ndvi.frz.srs$srs2, by= list(ndvi.frz.srs$month), FUN= mean); names(ndvi.frz.srs ) <- c("month", "srs.frz")

ndvi.frz.ms <- ndvi.df[which(ndvi.df$ms.events == 1),]
ndvi.frz.ms <- aggregate(ndvi.frz.ms$srs6, by= list(ndvi.frz.ms$month), FUN= mean); names(ndvi.frz.ms ) <- c("month", "ms.frz")

ndvi.frz.monthly <- merge(ndvi.frz.srs,ndvi.frz.ts1, all=T)
ndvi.frz.monthly <-merge (ndvi.frz.monthly,ndvi.frz.ms, all=T )
rm( ndvi.frz.ts1, ndvi.frz.srs, ndvi.frz.ms)

# Normal Values:
install.packages('doBy')
library(doBy)

ndvi.norm.ts1 <- ndvi.df[which(ndvi.df$ts.events == 0),]
ndvi.norm.ts1 <- summaryBy(ts1~ month, data=ndvi.norm.ts1,  FUN= function(x){c(mean = mean(x), sd = sd(x)) })

ndvi.norm.srs <- ndvi.df[which(ndvi.df$srs.events == 0),]
ndvi.norm.srs <- summaryBy(srs2~ month, data=ndvi.norm.srs,  FUN= function(x){c(mean = mean(x), sd = sd(x)) })

ndvi.norm.ms <- ndvi.df[which(ndvi.df$ms.events == 0),]
ndvi.norm.ms <- summaryBy(srs6~ month, data=ndvi.norm.ms,  FUN= function(x){c(mean = mean(x), sd = sd(x)) })

ndvi.monthly <- cbind(ndvi.norm.srs, ndvi.norm.ts1[,2:3], ndvi.norm.ms[2:3]) # merges datasets

rm(ndvi.norm.ts1 ,ndvi.norm.srs,  ndvi.norm.ms)

ndvi.monthly <-merge( ndvi.monthly,ndvi.frz.monthly, all=T )
rm(ndvi.frz.monthly)

# caculate differences:
ndvi.monthly$diff.srs <- ((ndvi.monthly$srs2.mean - ndvi.monthly$srs.frz)/ ndvi.monthly$srs2.mean*100)*-1
ndvi.monthly$diff.ts <- ((ndvi.monthly$ts1.mean - ndvi.monthly$ts.frz)/ ndvi.monthly$ts1.mean*100)*-1
ndvi.monthly$diff.ms <- ((ndvi.monthly$srs6.mean - ndvi.monthly$ms.frz)/ ndvi.monthly$srs6.mean*100)*-1

#___________________________________________FIGURE:NDVI_________________________________________________
par(mfrow=c(1,1), mai=c(1.4,1.4,0.5,0.5), cex=1.1)
plot( ndvi.monthly$month, ndvi.monthly$srs2.mean, ylim=c(0, 5), cex=2, lwd=2,
      xlab="Month", ylab= ("Mean Annual NDVI"))
points (ndvi.monthly$month, ndvi.monthly$ts1.mean, col="grey", cex=2, lwd=4)
points (ndvi.monthly$month, ndvi.monthly$ts6.mean, col="blue", cex=2, lwd=2)

points(ndvi.monthly$month, ndvi.monthly$srs.frz, pch=8, cex=2, lwd=4)
points(ndvi.monthly$month, ndvi.monthly$ts.frz, pch=8, col="grey", cex=2, lwd=2)
points(ndvi.monthly$month, ndvi.monthly$ms.frz, pch=8, col="blue", cex=2, lwd=2)

legend(4, 0.77, legend=c("SRS-2", "TS-1", "srS-6"), pch=c(1, 1, 1), col=c("black","grey", "blue"), 
       bty="n", cex=1.1)

legend(6, 0.77, legend=c("Cold Events"), pch=c(8), col=c( "black"), 
       bty="n", cex=1.1)

#___________________________________________TIMESERIES_________________________________________________
# srs

library(forecast)
auto.arima(ndvi.df$srs2, stationary=T,ic=c("aicc", "aic", "bic"), stepwise=TRUE, trace=T,
           test=c("kpss","adf","pp"), seasonal.test=c("ocsb","ch"),
           allowdrift=TRUE, allowmean=TRUE, lambda=NULL, parallel=FALSE)

arima.ndvi.srs2 <- arima(ndvi.df$srs2, order=c(1,0,0), xreg=ndvi.df$srs.events ,method = "ML")
round((1-pnorm(abs(arima.ndvi.srs2$coef)/sqrt(diag(arima.ndvi.srs2$var.coef))))*2,3) 

# TS-1
auto.arima(ndvi.df$ts1, stationary=T,ic=c("aicc", "aic", "bic"), stepwise=TRUE, trace=T,
           test=c("kpss","adf","pp"), seasonal.test=c("ocsb","ch"),
           allowdrift=TRUE, allowmean=TRUE, lambda=NULL, parallel=FALSE)

arima.ndvi.ts1 <- arima(ndvi.df$ts1, order=c(1,0,0), xreg=ndvi.df$ts.events ,method = "ML")
round((1-pnorm(abs(arima.ndvi.ts1$coef)/sqrt(diag(arima.ndvi.ts1$var.coef))))*2,3) 

# srS-6

auto.arima(ndvi.df$srs6, stationary=T,ic=c("aicc", "aic", "bic"), stepwise=TRUE, trace=T,
           test=c("kpss","adf","pp"), seasonal.test=c("ocsb","ch"),
           allowdrift=TRUE, allowmean=TRUE, lambda=NULL, parallel=FALSE)

arima.ndvi.srs6 <- arima(ndvi.df$srs6, order=c(2,0,2), xreg=ndvi.df$ms.events ,method = "ML")
round((1-pnorm(abs(arima.ndvi.srs6$coef)/sqrt(diag(arima.ndvi.srs6$var.coef))))*2,3) 

# SAVE ARIMA model infor to data.frame:
#SRS:
ndvi.arima.srs<- as.data.frame(arima.ndvi.srs2$coef)
srs.n <-as.data.frame((1-pnorm(abs(arima.ndvi.srs2$coef)/sqrt(diag(arima.ndvi.srs2$var.coef))))*2)
srs.se <- as.data.frame(sqrt(diag(arima.ndvi.srs2$var.coef)))
ndvi.arima.srs<- cbind(ndvi.arima.srs , srs.n, match(rownames(ndvi.arima.srs), rownames(srs.n)))
ndvi.arima.srs <- cbind(ndvi.arima.srs ,srs.se, match(rownames(ndvi.arima.srs), rownames(srs.se)))

ndvi.arima.srs <- ndvi.arima.srs[, -c(3,5)];
ndvi.arima.srs$Parameters <- rownames(ndvi.arima.srs)
ndvi.arima.srs <- ndvi.arima.srs[, c(4,1,3,2)]
ndvi.arima.srs[, c(2:4)] <- round(ndvi.arima.srs[, c(2:4)], 3)
names(ndvi.arima.srs) <- c('Parameter', 'Coefficents', 'S.E', 'p-value')
ndvi.arima.srs[3, 1] <- "Cold Events" ; rownames(ndvi.arima.srs) <-NULL

#TS-1:
ndvi.arima.ts1<- as.data.frame(arima.ndvi.ts1$coef)
ts1.n <-as.data.frame((1-pnorm(abs(arima.ndvi.ts1$coef)/sqrt(diag(arima.ndvi.ts1$var.coef))))*2)
ts1.se <- as.data.frame(sqrt(diag(arima.ndvi.ts1$var.coef)))
ndvi.arima.ts1<- cbind(ndvi.arima.ts1 , ts1.n, match(rownames(ndvi.arima.ts1), rownames(ts1.n)))
ndvi.arima.ts1 <- cbind(ndvi.arima.ts1 ,ts1.se, match(rownames(ndvi.arima.ts1), rownames(ts1.se)))

ndvi.arima.ts1 <- ndvi.arima.ts1[, -c(3,5)]
ndvi.arima.ts1$Parameters <- rownames(ndvi.arima.ts1)
ndvi.arima.ts1 <- ndvi.arima.ts1[, c(4,1,3,2)]
ndvi.arima.ts1[, c(2:4)] <- round(ndvi.arima.ts1[, c(2:4)], 3)
names(ndvi.arima.ts1) <- c('Parameter', 'Coefficents', 'S.E', 'p-value')
ndvi.arima.ts1[3, 1] <- "Cold Events" ; rownames(ndvi.arima.ts1) <-NULL

#SRS-6:
ndvi.arima.srs6<- as.data.frame(arima.ndvi.srs6$coef)
srs6.n <-as.data.frame((1-pnorm(abs(arima.ndvi.srs6$coef)/sqrt(diag(arima.ndvi.srs6$var.coef))))*2)
srs6.se <- as.data.frame(sqrt(diag(arima.ndvi.srs6$var.coef)))
ndvi.arima.srs6<- cbind(ndvi.arima.srs6 , srs6.n, match(rownames(ndvi.arima.srs6), rownames(srs6.n)))
ndvi.arima.srs6 <- cbind(ndvi.arima.srs6 ,srs6.se, match(rownames(ndvi.arima.srs6), rownames(srs6.se)))

ndvi.arima.srs6 <- ndvi.arima.srs6[, -c(3,5)]
ndvi.arima.srs6$Parameters <- rownames(ndvi.arima.srs6)
ndvi.arima.srs6 <- ndvi.arima.srs6[, c(4,1,3,2)]
ndvi.arima.srs6[, c(2:4)] <- round(ndvi.arima.srs6[, c(2:4)], 3)
names(ndvi.arima.srs6) <- c('Parameter', 'Coefficents', 'S.E', 'p-value')
ndvi.arima.srs6[6, 1] <- "Cold Events" ; rownames(ndvi.arima.srs6) <-NULL

write.csv(ndvi.arima.srs, "~/git/Cold-Events/Fluxes/arima_results_ndvi_srs.csv")
write.csv(ndvi.arima.ts1, "~/git/Cold-Events/Fluxes/arima_results_ndvi_ts.csv")
write.csv(ndvi.arima.srs6, "~/git/Cold-Events/Fluxes/arima_results_ndvi_ms.csv")

srs.ndvi.fitted <- fitted(arima(ndvi.df$srs2, order=c(1,0,0), xreg=ndvi.df$srs.events ,method = "ML"))
ts1.ndvi.fitted <- fitted(arima(ndvi.df$ts1, order=c(1,0,0), xreg=ndvi.df$ts.events ,method = "ML"))
srs6.ndvi.fitted <- fitted(arima(ndvi.df$srs6, order=c(2,0,2), xreg=ndvi.df$ms.events ,method = "ML"))

# ______________________________________________ ARIMA NDVI MODEL ______________________________________________
par(mfrow=c(3,1), mai=c(1, 1.5, 0.25, 0.25), tck=0.02, cex=1.1)
plot(ndvi.df$date, ndvi.df$srs2, ylim=c(0.20, 1), xlab="", ylab=" SRS-2  NDVI", xlim=c(as.Date('2009-01-01'), as.Date('2013-01-01')))
lines(ndvi.df$date, srs.ndvi.fitted, col="black", typ="l",lty=2)
abline(v=as.Date('2009-01-01'), lty=2)
abline(v=as.Date('2009-02-01'), lty=2)
abline(v=as.Date('2010-01-01'), lty=2)
abline(v=as.Date('2010-02-01'), lty=2)
abline(v=as.Date('2010-03-01'), lty=2)
abline(v=as.Date('2010-12-01'), lty=2)
abline(v=as.Date('2011-01-01'), lty=2)
abline(v=as.Date('2012-01-01'), lty=2)
mtext(text="a.", side=3, adj=0.01, cex=1.1, outer=F, line=-1)
plot(ndvi.df$date, ndvi.df$ts1, col="dimgrey",ylim=c(0.20, 1), xlab="", ylab=" TS-1  NDVI")
lines(ndvi.df$date,ts1.ndvi.fitted, col="dimgrey", typ="l",lty=2)
abline(v=as.Date('2009-01-01'), lty=2)
abline(v=as.Date('2009-02-01'), lty=2)
abline(v=as.Date('2009-03-01'), lty=2)
abline(v=as.Date('2010-01-01'), lty=2)
abline(v=as.Date('2010-02-01'), lty=2)
abline(v=as.Date('2010-03-01'), lty=2)
abline(v=as.Date('2010-12-01'), lty=2)
abline(v=as.Date('2011-01-01'), lty=2)
abline(v=as.Date('2011-03-01'), lty=2)
abline(v=as.Date('2012-01-01'), lty=2)
abline(v=as.Date('2012-03-01'), lty=2)
mtext(text="b.", side=3, adj=0.01, cex=1.1, outer=F, line=-1)
plot(ndvi.df$date,ndvi.df$srs6, col="blue",ylim=c(0.20, 1), xlab="", ylab=" SRS-6  NDVI", )
lines(ndvi.df$date,srs6.ndvi.fitted, col="blue", typ="l",lty=2)
abline(v=as.Date('2010-01-01'), lty=2)
abline(v=as.Date('2010-12-01'), lty=2)
abline(v=as.Date('2012-01-01'), lty=2)
mtext(text="c.", side=3, adj=0.01, cex=1.1, outer=F, line=-1)

