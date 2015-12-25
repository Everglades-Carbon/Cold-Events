# Author: Sparkle L. Malone
# Date: August 2015
# Objective: Identify days that are freeze events at all three sites for the PRISM analysis:

rm(list=ls())
#Import datafiles and compile table of interest
setwd("~/git/Cold-Events/Fluxes")

srs <- read.csv("SRS_freeze.csv")
ts <- read.csv("TS_freeze.csv")
ms <- read.csv("Mangroves_freeze.csv")

# Uses file from the CE_2 code:
srs.frz <- srs[which(srs$freeze == 1),]; srs.frz <- aggregate(srs.frz$freeze, by= list(srs.frz$date), FUN= "mean" ); names(srs.frz) <- c("date", "srs")
ts.frz <- ts[which(ts$freeze == 1),]; ts.frz <- aggregate(ts.frz$freeze, by= list(ts.frz$date), FUN= "mean" ); names(ts.frz) <- c("date", "ts")
ms.frz <- ms[which(ms$freeze == 1),]; ms.frz <- aggregate(ms.frz$freeze, by= list(ms.frz$date), FUN= "mean" ); names(ms.frz) <- c("date", "ms")

# Merges files:
Frz.dates <- merge(ts.frz, srs.frz, all=T)
Frz.dates <- merge(Frz.dates, ms.frz, all=T)

# change all NA's to 0:
Frz.dates[is.na(Frz.dates)] <- 0

# Sum events:
Frz.dates$sum <- rowSums(Frz.dates[ ,2:4])

# Subset events:
all.frz.dates <- Frz.dates[which(Frz.dates$sum == 3),]

Frz.dates$date <- as.Date(Frz.dates$date)
Frz.dates$yearmon <- format(Frz.dates$date, "%Y-%m")
frz.events.table <- aggregate(Frz.dates[ , c(2:4)], by=list(Frz.dates$yearmon), FUN=sum)
frz.events.table <-frz.events.table[-c(1:2), c(1, 3, 2,4)]
rownames(frz.events.table) <-NULL
names(frz.events.table) <- c("Year-Month", "SRS-2", "TS-1", "TS-6")
write.csv(frz.events.table, "~/git/Cold-Events/figures/Site-events_table.csv")

library(zoo)
frz.events.table[,1]<- as.yearmon(frz.events.table[,1])

par(mfrow=c(3,1), mai=c(0.6, 1.5,0.5, 0), cex=1.2)

barplot(frz.events.table[,3], col="dimgrey", border=NULL,ylab="TS-1 Events", ylim=c(0, 10),xlab="")
mtext(text="a.", side=3, adj=0.02, cex=1.1, outer=F, line=-1)
barplot(frz.events.table[,2], col="black", border=NULL,ylab="SRS-2 Events", ylim=c(0, 10),xlab="")
mtext(text="b.", side=3, adj=0.02, cex=1.1, outer=F, line=-1)
par(mai=c(1.5, 1.5,0.05, 0))
barplot(frz.events.table[,4], names.arg =frz.events.table[,1],las=3, col="blue", border=NULL,ylab="SRS-6 Events", ylim=c(0, 10) )
mtext(text="c.", side=3, adj=0.02, cex=1.1, outer=F, line=-1)