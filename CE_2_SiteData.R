# Author: Sparkle L. Malone
# Date: August 2015

# Fit light response curves
rm(list=ls())

#Import datafiles and compile table of interest
setwd("~/git/Cold-Events/Fluxes")

srs <- read.csv("SRS_freeze.csv")
ts <- read.csv("TS_freeze.csv")
ms <- read.csv("Mangroves_freeze.csv")

#Creates a parameter file to store all the coefficients in: 
parameters.frz<- data.frame(
  site=character(),
  freeze=character(),
  a1=numeric(),
  ax=numeric(),
  r=numeric(),
  a=numeric(),
  b=numeric(), 
    stringsAsFactors=FALSE, row.names=NULL)

# Creates Freeze files for each site:

# Freeze models (SITE):
# SRS
SRS.frz<-subset(srs, freeze == 1)
night.SRS.frz = nls(NEEnight ~ (a * exp(b*TA)), 
                           SRS.frz, start=list(a=0.4 , b=0.01 ), na.action=na.exclude, trace=F)
day.SRS.frz = nls(NEEDay ~ (a1 *srs.par.filled * ax)/(a1 * srs.par.filled + ax) + r, 
                          SRS.frz, start=list(a1=0.01 , ax= 1, r= 1), na.action=na.exclude, trace=F)
summary(night.SRS.frz)
summary(day.SRS.frz)

# add data to file:
parameters.frz[1,1]<- ('srs')
parameters.frz[1,2]<- ('1')
parameters.frz[1,3:7] <-rbind(merge(as.data.frame( t(coef(summary(day.SRS.frz)) [1:3, 1])), 
                                    as.data.frame(t(coef(summary(night.SRS.frz)) [1:2, 1])))) 


#____________________________

# TS
TS.frz<-subset(ts, freeze == 1)
night.TS.frz = nls(NEEnight ~ (a * exp(b*TA)), 
                    TS.frz, start=list(a=0.4 , b=0.01 ), na.action=na.exclude, trace=F)
day.TS.frz = nls(NEEDay ~ (a1 *ts.par.filled * ax)/(a1 * ts.par.filled + ax) + r, 
                  TS.frz, start=list(a1=0.01 , ax= 1, r= 1), na.action=na.exclude, trace=F)
summary(night.TS.frz)
summary(day.TS.frz)
# add data to file:
parameters.frz[2,1]<- ('ts')
parameters.frz[2,2]<- ('1')
parameters.frz[2,3:7] <-rbind(merge(as.data.frame( t(coef(summary(day.TS.frz)) [1:3, 1])), 
                                    as.data.frame(t(coef(summary(night.TS.frz)) [1:2, 1])))) 


# Mangrove (TS6)

ms.frz<-subset(ms, freeze == 1)
night.ms.frz = nls(NEEnight ~ (a * exp(b*TA)), 
                  ms.frz, start=list(a=0.4 , b=0.01 ), na.action=na.exclude, trace=F, nls.control(maxiter = 100, tol = 1e-05, minFactor = 1/1024,
                                                                                                  printEval = FALSE, warnOnly = FALSE))
day.ms.frz = nls(NEEDay ~ (a1 *ms.par.filled * ax)/(a1 * ms.par.filled + ax) + r, 
                 ms.frz, start=list(a1=0.0007 , ax= -0.015, r= -8.23), na.action=na.exclude, trace=F,nls.control(maxiter = 100, tol = 1e-05, minFactor = 1/1024,
                                                                                                      printEval = FALSE, warnOnly = FALSE))

summary(night.ms.frz)
summary(day.ms.frz)

plot(ms.frz$TA, ms.frz$NEEnight ); abline(night.ms.frz )
plot(ms.frz$ms.par.filled , ms.frz$NEEDay)

# add data to file:
parameters.frz[3,1]<- ('ms')
parameters.frz[3,2]<- ('1')
parameters.frz[3,3:7] <-rbind(merge(as.data.frame( t(coef(summary(day.ms.frz)) [1:3, 1])), 
                                    as.data.frame(t(coef(summary(night.ms.frz)) [1:2, 1])))) 

# __________________________Non-Freeze Data_________________________
# SRS
srs$winter <- 0; srs$winter[srs$month == 1 | srs$month == 2| srs$month == 12]<-1
srs.n<-subset(srs, freeze == 0 & winter==1)

night.SRS = nls(NEEnight ~ (a * exp(b*TA)), 
                    srs.n, start=list(a=0.4 , b=0.01 ), na.action=na.exclude, trace=F)
day.SRS = nls(NEEDay ~ (a1 *srs.par.filled * ax)/(a1 * srs.par.filled + ax) + r, 
                  srs.n, start=list(a1=0.01 , ax= 1, r= 1), na.action=na.exclude, trace=F)
summary(night.SRS)
summary(day.SRS)

# add data to file:
parameters.frz[4,1]<- ('srs')
parameters.frz[4,2]<- ('0')
parameters.frz[4,3:7] <-rbind(merge(as.data.frame( t(coef(summary(day.SRS)) [1:3, 1])), 
                                    as.data.frame(t(coef(summary(night.SRS)) [1:2, 1])))) 


# TS
ts$winter <- 0; ts$winter[ts$month == 1 | ts$month == 2| ts$month == 12]<-1
ts.n <-subset(ts, freeze == 0 & winter ==1) 
night.TS = nls(NEEnight ~ (a * exp(b*TA)), 
               ts.n, start=list(a=0.4 , b=0.01 ), na.action=na.exclude, trace=F)
day.TS = nls(NEEDay ~ (a1 *ts.par.filled * ax)/(a1 * ts.par.filled + ax) + r, 
             ts.n, start=list(a1=0.01 , ax= 1, r= 1), na.action=na.exclude, trace=F)

summary(night.TS)
summary(day.TS)

# add data to file:
parameters.frz[5,1]<- ('ts')
parameters.frz[5,2]<- ('0')
parameters.frz[5,3:7] <-rbind(merge(as.data.frame( t(coef(summary(day.TS)) [1:3, 1])), 
                                    as.data.frame(t(coef(summary(night.TS)) [1:2, 1])))) 

# Mangrove (TS6)
ms$winter <- 0; ms$winter[ms$month == 1 | ms$month == 2| ms$month == 12]<-1
ms.n <-subset(ms, freeze == 0 & winter ==1) 

night.ms = nls(NEEnight ~ (a * exp(b*TA)), 
                   ms.n, start=list(a=0.4 , b=0.01 ), na.action=na.exclude, trace=F,nls.control(maxiter = 10000, tol = 1e-05, minFactor = 0,
                                                                                                printEval = FALSE, warnOnly = FALSE))
day.ms = nls(NEEDay ~ (a1 *ms.par.filled * ax)/(a1 * ms.par.filled + ax) + r, 
                 ms.n, start=list(a1=0.01 , ax= 1, r= 1), na.action=na.exclude, trace=F,nls.control(maxiter = 10000, tol = 1e-05, minFactor = 0,
                                                                                                    printEval = FALSE, warnOnly = FALSE))

summary(day.ms)
summary(night.ms)

# add data to file:
parameters.frz[6,1]<- ('ms')
parameters.frz[6,2]<- ('0')
parameters.frz[6,3:7] <-rbind(merge(as.data.frame( t(coef(summary(day.ms)) [1:3, 1])), 
                                    as.data.frame(t(coef(summary(night.ms)) [1:2, 1])))) 

rm( SRS.frz, srs.n, TS.frz, ts.n, ms.frz, ms.n)

#_________________________________________________________________________________________

# add model parameters to files:
parameters.frz$winter <- 1 # adds winter to the parameter file
parameters.frz$freeze <- as.factor(parameters.frz$freeze) 
srs$site <- "srs"; ts$site <- "ts"; ms$site <- "ms" # adds the site to the file


# Merge parameter and data files
srs.model <- merge(srs, parameters.frz, 
                   by=intersect(c('site', 'freeze', 'winter'), c('site', 'freeze', 'winter')), all=F)
ts.model <- merge(ts, parameters.frz, 
                  by=intersect(c('site', 'freeze', 'winter'), c('site', 'freeze', 'winter')), all=F)
ms.model <- merge(ms, parameters.frz, 
                  by=intersect(c('site', 'freeze', 'winter'), c('site', 'freeze', 'winter')), all=F)

# Estimates for models
srs.model$x <- 0; srs.model$x[srs.model[12] == 'day'] <- 1
ts.model$x <- 0; ts.model$x[ts.model[12] == 'day'] <- 1
ms.model$x <- 0; ms.model$x[ms.model[10] == 'day'] <- 1

nee.model.day <- function(a1, ax, r, p){
    y = (a1 * p * ax)/(a1 * p + ax) + r
  print(y)} 
  
nee.model.night <- function(a , b , t){
  y = (a * exp(b*t))
  print(y)}

srs.model$est.nee[srs.model$x == 1] <- mapply(srs.model[19],
                            srs.model[20],
                            srs.model[21],
                            srs.model[10], FUN= nee.model.day)
srs.model$est.nee[srs.model$x == 0]<- mapply(srs.model[22],
                                             srs.model[23],
                                             srs.model[6], FUN= nee.model.night)

ts.model$est.nee[ts.model$x == 1] <- mapply(ts.model[19],
                                              ts.model[20],
                                              ts.model[21],
                                              ts.model[10], FUN= nee.model.day)
ts.model$est.nee[ts.model$x == 0]<- mapply(ts.model[22],
                                             ts.model[23],
                                             ts.model[6], FUN= nee.model.night)

ms.model$est.nee[ms.model$x == 1] <- mapply(ms.model[17],
                                            ms.model[18],
                                            ms.model[19],
                                            ms.model[9], FUN= nee.model.day)
ms.model$est.nee[ms.model$x == 0]<- mapply(ms.model[20],
                                           ms.model[21],
                                           ms.model[7], FUN= nee.model.night)

param.2 <- parameters.frz[which(parameters.frz$freeze == 0),]

names(param.2) <-c('site','freeze',	'a1.norm', 'ax.norm', 'r.norm',	'a.norm',	'b.norm',	'winter')

# merges non-frezze model parameters with th file:
srs.model <- merge(srs.model, param.2, by=intersect(c('site', 'winter'),c('site', 'winter')), all=F)
ts.model <- merge(ts.model, param.2, by=intersect(c('site', 'winter'),c('site', 'winter')), all=F)
ms.model <- merge(ms.model, param.2, by=intersect(c('site', 'winter'),c('site', 'winter')), all=F)

#Estimates CO2 loss
srs.model$est.nee.loss[srs.model$x == 1] <- mapply(srs.model[27],
                                              srs.model[28],
                                              srs.model[29],
                                              srs.model[10], FUN= nee.model.day)
srs.model$est.nee.loss[srs.model$x == 0]<- mapply(srs.model[30],
                                             srs.model[31],
                                             srs.model[6], FUN= nee.model.night)

ts.model$est.nee.loss[ts.model$x == 1] <- mapply(ts.model[27],
                                            ts.model[28],
                                            ts.model[29],
                                            ts.model[10], FUN= nee.model.day)
ts.model$est.nee.loss[ts.model$x == 0]<- mapply(ts.model[30],
                                           ts.model[31],
                                           ts.model[6], FUN= nee.model.night)

ms.model$est.nee.loss[ms.model$x == 1] <- mapply(ms.model[25],
                                            ms.model[26],
                                            ms.model[27],
                                            ms.model[9], FUN= nee.model.day)
ms.model$est.nee.loss[ms.model$x == 0]<- mapply(ms.model[28],
                                           ms.model[29],
                                           ms.model[7], FUN= nee.model.night)

ms.model$nee.diff <-  ms.model$NEE - ms.model$est.nee
ts.model$nee.diff <- ts.model$NEE - ts.model$est.nee
srs.model$nee.diff <-  srs.model$NEE - srs.model$est.nee

# removes all values when not a freeze event:

ms.model$nee.diff[ms.model$freeze.x == 0] <-0
ts.model$nee.diff[ts.model$freeze.x == 0] <-0
srs.model$nee.diff[srs.model$freeze.x == 0] <-0

# Positive value = C gained and negative values = C lost 
sum(na.omit(ms.model$nee.diff))* 44/1000000 * 1800
sum(na.omit(ts.model$nee.diff))* 44/1000000 * 1800
sum(na.omit(srs.model$nee.diff))* 44/1000000 * 1800

# Site Level models:

# Day models (frz)
site.models <- data.frame()
site.models[1:41, 1] <- seq(0, 2000, 50); names(day.models)<-c('par') 
site.models$TA <- seq(-5, 40, 1.125)

site.models$srs.frz.day <- mapply(parameters.frz[1,3],
                             parameters.frz[1,4],
                             parameters.frz[1,5],
                             site.models[1], FUN= nee.model.day)
site.models$ts.frz.day <- mapply(parameters.frz[2,3],
                             parameters.frz[2,4],
                             parameters.frz[2,5],
                             site.models[1], FUN= nee.model.day)

site.models$ms.frz.day <- mapply(parameters.frz[3,3],
                             parameters.frz[3,4],
                             parameters.frz[3,5],
                             site.models[1], FUN= nee.model.day)

# Day models (non-frz)
site.models$srs.norm.day <- mapply(parameters.frz[4,3],
                                  parameters.frz[4,4],
                                  parameters.frz[4,5],
                                  site.models[1], FUN= nee.model.day)
site.models$ts.norm.day <- mapply(parameters.frz[5,3],
                                 parameters.frz[5,4],
                                 parameters.frz[5,5],
                                 site.models[1], FUN= nee.model.day)

site.models$ms.norm.day <- mapply(parameters.frz[6,3],
                                 parameters.frz[6,4],
                                 parameters.frz[6,5],
                                 site.models[1], FUN= nee.model.day)

# Night models (frz)
site.models$srs.frz.night<- mapply(parameters.frz[1, 6],
                                  parameters.frz[1,7],
                                  site.models[2],
                                  FUN= nee.model.night)
site.models$ts.frz.night<- mapply(parameters.frz[2, 6],
                                   parameters.frz[2,7],
                                   site.models[2],
                                   FUN= nee.model.night)
site.models$ms.frz.night<- mapply(parameters.frz[3, 6],
                                  parameters.frz[3,7],
                                  site.models[2],
                                  FUN= nee.model.night)

# Night models (Non-frz)
site.models$srs.norm.night<- mapply(parameters.frz[4, 6],
                                   parameters.frz[4,7],
                                   site.models[2],
                                   FUN= nee.model.night)
site.models$ts.norm.night<- mapply(parameters.frz[5, 6],
                                  parameters.frz[5,7],
                                  site.models[2],
                                  FUN= nee.model.night)
site.models$ms.norm.night<- mapply(parameters.frz[6, 6],
                                  parameters.frz[6,7],
                                  site.models[2],
                                  FUN= nee.model.night)

#__________________________________________Create Figure for models __________________________________________
# Plots separate for Freshwater marsh and Mangrove ecotone due to differences in range
par(tck=0.02, mfrow=c(2,2), 
    mai=c(1.3,1.5,0.1,0.25), cex=1.3)

# Day Figures
plot(site.models$V1,site.models$srs.frz.day , typ="l", ylim=c(-2, 2),
     lwd=4, ylab=expression(paste("CO"[2]," Flux ("~mu, "mol m"^"-2","s"^"-1",")")), 
     xlab=expression(paste("PAR ("~mu, "mol m"^"-2","s"^"-1",")")))
lines(site.models$V1,site.models$srs.norm.day , pch=2, lty=2, lwd=4)

lines(site.models$V1,site.models$ts.norm.day , typ="l", col="dimgrey", lwd=2)
lines(site.models$V1,site.models$ts.frz.day , typ="l", col="dimgrey",lty=2, lwd=3)
mtext(text="a.", side=3, adj=0.08, cex=1.2, outer=F, line=-1)

legend(20, 1.8,legend=c('SRS-2  (normal)', expression(paste('SRS-2 (T < 5'^o,'C)')), 'TS-1 (normal)', 
                        expression(paste('TS-2 (T < 5'^o,'C)'))), 
                        col=c('black', 'black', 'dimgrey', 'dimgrey'), lwd=4, lty=c(2,1,2,1), bty="n")
# Mangroves flux model figure component:
plot(site.models$V1, site.models$ms.frz.day, typ="l", col="blue", lwd=3,
     ylab="", ylim=c(-8, 1), xlab=expression(paste("PAR ("~mu, "mol m"^"-2","s"^"-1",")")))
lines(site.models$V1,site.models$ms.norm.day, typ="l", col="blue", lty=2, lwd=3)

legend(100, 1,legend=c('SRS-6 (normal)', expression(paste('SRS-6 (T < 5'^o,'C)'))), col='blue',
       lwd=4, lty=c(2,1), bty="n")
mtext(text="b.", side=3, adj=0.08, cex=1.2, outer=F, line=-1)

# Night Figures
plot(site.models$srs.frz.night , typ="l", ylim=c(0.5, 2), lwd=4,
     ylab= expression(paste("CO"[2]," Flux ("~mu, "mol m"^"-2","s"^"-1",")")), xlab=expression(paste(" Temperature ("~degree,"C)")))
lines(site.models$srs.norm.night , pch=2, lty=2, lwd=4)
lines(site.models$ts.norm.night , typ="l", col="dimgrey", lwd=3)
lines(site.models$ts.frz.night , typ="l", col="dimgrey",lty=2, lwd=3)
mtext(text="c.", side=3, adj=0.08, cex=1.2, outer=F, line=-1)

# Mangroves flux model figure component:
plot(site.models$ms.frz.night , typ="l", col="blue", lwd=3,
     ylab= "", xlab=expression(paste(" Temperature ("~degree,"C)")))
lines(site.models$TA,site.models$ms.norm.night , typ="l", col="blue", lty=2, lwd=3)
mtext(text="d.", side=3, adj=0.08, cex=1.2, outer=F, line=-1)

#______________________________________________________________________________________________________________
# NOTE: the ts and ms moels are questionable!
# Changes in PAR on cold versus Freeze days:

# Separate time:
library(splitstackshape)
srs<- concat.split(srs, 12, sep=" ", drop=T)
ts<- concat.split(ts, 12, sep=" ", drop=T)
ms<- concat.split(ms, 10, sep=" ", drop=T)

# Freeze
srs.par.frz <- srs[which(srs$freeze == 1),]; srs.par.frz <- aggregate(srs.par.frz$srs.par.filled, by=list(srs.par.frz$datetime_2), FUN= mean); plot(srs.par.frz[,2] )
ts.par.frz <- ts[which(ts$freeze == 1),]; ts.par.frz <- aggregate(ts.par.frz$ts.par.filled, by=list(ts.par.frz$datetime_2), FUN= mean); plot(ts.par.frz[,2] )
ms.par.frz <- ms[which(ms$freeze == 1),]; ms.par.frz <- aggregate(ms.par.frz$ms.par.filled, by=list(ms.par.frz$datetime_2), FUN= mean); plot(ms.par.frz[,2] )

frz.par <- cbind(srs.par.frz,ts.par.frz[,2],ms.par.frz[,2])

names(frz.par)<- c("time", "srs.frz", 'ts.frz', 'ms.frz'); rm(srs.par.frz,ts.par.frz,ms.par.frz )

# Non-Frz

srs.par.norm <- srs[which(srs$freeze == 0 & srs$winter == 1),]; srs.par.norm <- aggregate(srs.par.norm$srs.par.filled, by=list(srs.par.norm$datetime_2), FUN= mean); plot(srs.par.norm[,2] )
ts.par.norm <- ts[which(ts$freeze == 0 & ts$winter == 1),]; ts.par.norm <- aggregate(ts.par.norm$ts.par.filled, by=list(ts.par.norm$datetime_2), FUN= mean); plot(ts.par.norm[,2] )
ms.par.norm <- ms[which(ms$freeze == 0 & ms$winter == 1),]; ms.par.norm <- aggregate(ms.par.norm$ms.par.filled, by=list(ms.par.norm$datetime_2), FUN= mean); plot(ms.par.norm[,2] )

norm.par <- cbind(srs.par.norm,ts.par.norm[,2],ms.par.norm[,2])
names(norm.par)<- c("time", "srs.norm", 'ts.norm', 'ms.norm'); rm(srs.par.norm,ts.par.norm,ms.par.norm )

Par.analysis <- merge(frz.par, norm.par); rm(norm.par, frz.par )

# format time column:
Par.analysis$time.test <-as.POSIXlt(Par.analysis$time, format="%H:%M:%S", "MST")
Par.analysis$time <- format(Par.analysis$time.test, format="%H:%M")
Par.analysis$time.test <- NULL

# calculate means:
Par.analysis$frz.par <- rowMeans(Par.analysis[, c(2:4)])
Par.analysis$Norm.par <- rowMeans(Par.analysis[, c(5:7)])

# PAR Figure
par(mfrow=c(1,1), mai= c (1.7, 1.5, 0.25, 0.25), cex=1.2)

plot(smooth(Par.analysis$Norm.par), xaxt='n', ylab=expression(paste("PAR ("~mu, "mol m"^"-2","s"^"-1",")")), 
     type="l", col="black", lty=2, lwd= 2, ylim=c(-0.1, 1500), xlab='Hour')
axis(1, at=seq(1, 48, 1), labels=Par.analysis$time)
lines(smooth(Par.analysis$frz.par), lty=1, lwd=1.5)
legend('topleft', legend= c('Average', expression(paste('T < 5'^o,'C'))), lty= c(2,1), bty="n", lwd=c(2,1))


# Summary
sum(Par.analysis$frz.par)-sum(Par.analysis$Norm.par)
#________________--______---__________-----_________Calcutae SE 


srs.par.frz<- srs[which(srs$freeze == 1),]; srs.par.frz2 <- aggregate(srs.par.frz$srs.par.filled, by=list(srs.par.frz$date), FUN= sum)
ts.par.frz<- ts[which(ts$freeze == 1),]; ts.par.frz2 <- aggregate(ts.par.frz$ts.par.filled, by=list(ts.par.frz$date), FUN= sum)
ms.par.frz<- ms[which(ms$freeze == 1),]; ms.par.frz2 <- aggregate(ms.par.frz$ms.par.filled, by=list(ms.par.frz$date), FUN= sum)

names(srs.par.frz2)<- c('date', 'srs.par')
names(ts.par.frz2)<- c('date', 'ts.par')
names(ms.par.frz2)<- c('date', 'ms.par')

frz.par2 <- merge(srs.par.frz2, ts.par.frz2, all=T)
frz.par3 <- merge(frz.par2, ms.par.frz2, all=T)

rm(frz.par2)

frz.par3$mean <-  rowMeans(frz.par3[, 2:4], na.rm=T)
frz.par3$par.diff <- frz.par3$mean- sum(Par.analysis$Norm.par)

sum(Par.analysis$Norm.par) # average daily sum of par on normal days
mean(frz.par3$par.diff) # Avg difference in par on freeze days from normal day mean
sqrt(var(frz.par3$par.diff))/sqrt(length(frz.par3$par.diff)) # SE in diff between freeze day PAR and mean normal day 
