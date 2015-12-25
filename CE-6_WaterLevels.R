# # Author: Sparkle L. Malone
# Date: August 2015
# Objective: Compile waterl level data from SRS-2, TS-1, and TS-6

rm(list=ls())

#Import datafiles and compile table of interest
setwd("~/git/Cold-Events/Fluxes/waterlevel")

srs.met <- read.csv('srs_metdata.csv')
srs.met$Date <- as.Date(srs.met$date) 
srs.met$datetime <- as.POSIXlt(srs.met$datetime,  "MST7MDT",format= "%Y-%m-%d %H:%M")

ts.met <- read.csv('ts_metdata.csv')
ts.met$Date <- as.Date(ts.met$date) 
ts.met$datetime <- as.POSIXlt(ts.met$datetime,  "MST7MDT",format= "%Y-%m-%d %H:%M")


ms.wl <- read.csv('1440115843_water_level.csv', skip=3, header=T)
ms.wl$Date <- as.Date(ms.wl$Date)
ms.wl$ms.wl <- ms.wl[,2] * 0.3048
ms.wl<- ms.wl[, c(1,5)]
# Need to aggregate TS-1 and SRS-2 data to create mean annula wl 

srs.wl <- aggregate( srs.met$Water.Level, by= list( srs.met$Date), FUN=mean); names(srs.wl) <- c('Date', 'srs.wl')
ts.wl <- aggregate( ts.met$Water.Level, by= list( ts.met$Date), FUN=mean); names(ts.wl) <- c('Date', 'ts.wl')

t1 <- merge(srs.wl, ts.wl)
wl <- merge(t1, ms.wl); rm(srs.met, ts.met, srs.wl, ts.wl, ms.wl, t1)
wl$Date <- as.Date(wl$Date)

# _______________________________WATER LEVEL FIGURE____________________________________________________

par(mfrow=c(1,1), tck=0.02, mai=c(0.75, 1.25, 0.25, 0.25))
plot(wl$Date, wl$srs.wl, type="l", lwd= 2, ylab="Water Level (m)", xlab="",
     ylim=c(-1.5, 0.8))
lines(wl$Date, wl$ts.wl, col= "dimgrey")
lines(wl$Date, wl$ms.wl, col= "blue")
legend( "bottomright", legend= c("SRS-2", "TS-1", "TS-6"), col=c("black", "dimgrey", "blue"), lty=1, bty="n", lwd=2)

# _____________________________________________________________________________________________________

lm.srs.ts <- lm(wl$srs.wl~ wl$ts.wl)
lm.ts.ms <- lm(wl$ts.wl~ wl$ms.wl)
lm.srs.ms <- lm(wl$srs.wl~ wl$ms.wl)

summary(lm.srs.ts)
summary(lm.ts.ms)
summary(lm.srs.ms)

# Format text elements for plots:
r2.srs.ts <- bquote(italic(R)^2 == .(format(summary(lm.srs.ts)$r.squared, digits=2)))
p.val.srs.ts <- bquote(italic(p-value) == .(format(summary(lm.srs.ts)$coefficients[,4][2] , digits=2)))
eq.srs.ts <- paste("y = ",round(summary(lm.srs.ts)$coefficients[1], 2)," - ", round(abs(summary(lm.srs.ts)$coefficients[2]), 2),"x", sep="" )

r2.ts.ms <- bquote(italic(R)^2 == .(format(summary(lm.ts.ms)$r.squared, digits=2)))
p.val.ts.ms  <- bquote(italic(p-value) == .(format(summary(lm.ts.ms)$coefficients[,4][2] , digits=2)))
eq.ts.ms <-paste("y = ",round(summary(lm.ts.ms)$coefficients[1], 2)," - ", round(abs(summary(lm.ts.ms)$coefficients[2]), 2),"x", sep="" )

r2.srs.ms <- bquote(italic(R)^2 == .(format(summary(lm.srs.ms)$r.squared, digits=2)))
p.val.srs.ms <- bquote(italic(p-value) == .(format(summary(lm.srs.ms)$coefficients[,4][2] , digits=2)))
eq.srs.ms <- paste("y = ",round(summary(lm.srs.ms)$coefficients[1], 2)," - ", round(abs(summary(lm.srs.ms)$coefficients[2]), 2),"x", sep="" )

p.val = bquote(italic(p) <0.001)

text(3, 9, labels=r2.label)
text(3, 8, labels=p.val)

par(mfrow=c(3, 1), mai=c(1.2, 1.2, 0.5, 0.25), cex=1.1, tck=0.04)
plot(wl$ts.wl, wl$srs.wl, pch=16, col="grey", ylab= "SRS-2 Water Level (m)", xlab="TS-1 Water Level (m)", xlim=c(-0.8, 0.8), ylim=c(-0.8, 0.8))
abline(lm.srs.ts, lwd=2)
text(0.4, -0.1, labels=eq.srs.ts)
text(0.4, -0.3, labels=r2.srs.ts)
text(0.4, -0.5, labels=p.val)
lines(c(-1, 1), c(-1, 1), col="gray") # one tow one line
mtext(text="a.", side=3, adj=0.01, cex=1.1, outer=F, line=-1)

plot(wl$ms.wl, wl$ts.wl, pch=16, col="grey", ylab= "TS-1 Water Level (m)", xlab="TS-6 Water Level (m)", xlim=c(-0.8, 0.8), ylim=c(-0.8, 0.8))
abline(lm.ts.ms , lwd=2)
text(0.4, -0.1, labels=eq.ts.ms)
text(0.4, -0.3, labels=r2.ts.ms)
text(0.4, -0.5, labels=p.val)
lines(c(-1, 1), c(-1, 1), col="gray") # one tow one line
mtext(text="b.", side=3, adj=0.01, cex=1.1, outer=F, line=-1)

plot(wl$ms.wl, wl$srs.wl, pch=16, col="grey", ylab= "SRS-2 Water Level (m)", xlab="TS-6 Water Level (m)", xlim=c(-0.8, 0.8), ylim=c(-0.8, 0.8))
abline(lm.srs.ms, lwd=2)
text(0.4, -0.1, labels=eq.srs.ms)
text(0.4, -0.3, labels=r2.srs.ms)
text(0.4, -0.5, labels=p.val)
lines(c(-1, 1), c(-1, 1), col="gray") # one tow one line
mtext(text="c.", side=3, adj=0.01, cex=1.1, outer=F, line=-1)


     

