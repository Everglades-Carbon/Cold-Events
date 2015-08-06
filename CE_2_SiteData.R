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

srs.model <- merge(srs.model, param.2, by=intersection(c('site', 'winter'),c('site', 'winter')), all=F)
ts.model <- merge(ts.model, param.2, by=intersection(c('site', 'winter'),c('site', 'winter')), all=F)
ms.model <- merge(ms.model, param.2, by=intersection(c('site', 'winter'),c('site', 'winter')), all=F)

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

ms.model$nee.diff[ms.model$freeze == 1] <- ms.model$est.nee.loss[ms.model$freeze == 1] - ms.model$NEE[ms.model$freeze == 1]
ts.model$nee.diff[ts.model$freeze == 1] <- ts.model$est.nee.loss[ts.model$freeze == 1] - ts.model$NEE[ts.model$freeze == 1]
srs.model$nee.diff[srs.model$freeze == 1] <- srs.model$est.nee.loss[srs.model$freeze == 1] - srs.model$NEE[srs.model$freeze == 1]



sum(na.omit(ms.model$nee.diff))*(44/1000000)*1800
sum(srs.model$nee.diff)*(44/1000000)*1800
sum(ts.model$nee.diff)*(44/1000000)*1800
