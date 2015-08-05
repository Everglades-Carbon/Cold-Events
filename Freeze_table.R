# OBJECTIVE: Freeze table showing the number of events at each site:

rm(list=ls())

#Import site (SRS2 TS1 Mangroves(TS6)):
setwd("/Users/sparklemalone/git/Cold-Events/Fluxes")

srs <- read.csv("SRS_freeze.csv")
ts <- read.csv("TS_freeze.csv")
ms <- read.csv("Mangroves_freeze.csv")

# Aggregate Tmin by date
library(plyr)
srs.day <- ddply(srs,.(date), summarize, date = date[which.min(TA)], TA.min.srs=min(TA))
ts.day <- ddply(ts,.(date), summarize, date = date[which.min(TA)], TA.min.ts=min(TA))
ms.day <- ddply(ms,.(date), summarize, date = date[which.min(TA)], TA.min.ms=min(TA))

# Create an Indicator for freeze events
freeze<- function (x){
  if (x <= 5){
    y=1}else{y=0
    }
}

srs.day$freeze.srs <- sapply(srs.day$TA.min.srs, freeze)
ts.day$freeze.ts <- sapply(ts.day$TA.min.ts, freeze)
ms.day$freeze.ms <- sapply(ms.day$TA.min.ms, freeze)

srs.freeze<-subset(srs.day, freeze.srs == 1 )
ts.freeze<-subset(ts.day, freeze.ts == 1 )
ms.freeze <- subset(ms.day, freeze.ms == 1 )

Freeze.1 <- merge(srs.freeze, ts.freeze, sort = T, all=T)
Freeze <- merge(Freeze.1, ms.freeze,  sort = T, all=T)

rm(srs.freeze, ts.freeze, ms.freeze, Freeze.1)

Freeze$date <- as.Date(Freeze$date, format= "%Y-%m-%d") # Format the date column
Freeze$day <- format(Freeze$date, "%d") # exract the day from the date...etc.
Freeze$month <- format(Freeze$date, "%m")
Freeze$year <- format(Freeze$date, "%Y")
Freeze$yearmon <- format(Freeze$date, "%Y-%m")
Freeze$count<- 1 # Adds an indicator 

library(plyr)
Freeze.table <- ddply(Freeze,.(yearmon), summarize, yearmon = yearmon[which.max(year)], 
                      freeze.srs=sum(na.omit(freeze.srs)), freeze.ts=sum(na.omit(freeze.ts)),
                      freeze.ms=sum(na.omit(freeze.ms)) )
names(Freeze.table) <- c("Year-Month ", 'SRS2', 'TS1', 'TS6')


setwd('~/git/Cold-Events/Fluxes/figures")

srs <- read.csv("SRS_freeze.csv")
ts <- read.csv("TS_freeze.csv")
ms <- read.csv("Mangroves_freeze.csv")')
write.csv(Freeze.table, 'freeze_table_sites.csv')

