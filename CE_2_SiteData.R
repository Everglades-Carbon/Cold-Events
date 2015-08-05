# Author: Sparkle L. Malone
# Date: August 2015

# Fit light response curves
rm(list=ls())

#Import datafiles and compile table of interest
setwd("/Users/sparklemalone/git/Cold-Events/Fluxes")

srs <- read.csv("SRS_freeze.csv")
ts <- read.csv("TS_freeze.csv")
ms <- read.csv("Mangroves_freeze.csv")

#Creates a parameter file to store all the coefficients in: 
parameters.frz<- data.frame(
  site=character(),
  condition=character(),
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
                  ms.frz, start=list(a=0.4 , b=0.01 ), na.action=na.exclude, trace=F)
day.ms.frz = nls(NEEDay ~ (a1 *ms.par.filled * ax)/(a1 * ms.par.filled + ax) + r, 
                 ms.frz, start=list(a1=0.01 , ax= 1, r= 1), na.action=na.exclude, trace=F)

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

# TS
ts.n <-subset(ts, freeze == 0)
night.TS = nls(NEEnight ~ (a * exp(b*TA)), 
               ts.n, start=list(a=0.4 , b=0.01 ), na.action=na.exclude, trace=F)
day.TS = nls(NEEDay ~ (a1 *ts.par.filled * ax)/(a1 * ts.par.filled + ax) + r, 
             ts.n, start=list(a1=0.01 , ax= 1, r= 1), na.action=na.exclude, trace=F)
summary(night.TS)
summary(day.TS)

# Mangrove (TS6)
ms$winter <- 0; ms$winter[ms$month == 1 | ms$month == 2| ms$month == 12]<-1
ms.n <-subset(ms, freeze == 0 & winter ==1) 

night.ms = nls(NEEnight ~ (a * exp(b*TA)), 
                   ms.n, start=list(a=0.4 , b=0.01 ), na.action=na.exclude, trace=F)
day.ms = nls(NEEDay ~ (a1 *ms.par.filled * ax)/(a1 * ms.par.filled + ax) + r, 
                 ms.n, start=list(a1=0.01 , ax= 1, r= 1), na.action=na.exclude, trace=F)
summary(night.ms)
summary(day.ms)


