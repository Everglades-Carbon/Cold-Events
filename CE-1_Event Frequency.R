# Author: Sparkle L. Malone
# Date: August 2015

library(devtools)
library("rio")

rm(list=ls())

# Import data files (Current  file extend to 2013).
setwd('~/git/Cold-Events/WeatherStation_Data')

evg <- read.csv('FCE_Everglades_ClimDB_data.csv')
fla <- read.csv('FCE_FlamingoRS_ClimDB_data.csv')
rp <- read.csv('FCE_RoyalPalmRS_ClimDB_data.csv')
tav <- read.csv('FCE_Tavernier_ClimDB_data.csv')

cold <- rbind(evg, fla, rp, tav)
rm(evg, fla, rp, tav)

# Format the date:
cold$delete.1 <-cold$Date/10000 
library(splitstackshape)
cold<-concat.split(cold, 12, sep=".", drop=T)
cold$year <- cold$delete.1_1; cold$delete.1_1<-cold$delete.1_2<- NULL

cold$delete <-(cold$Date-(round(cold$Date/10000,0)*10000))/100
cold <-concat.split(cold, 13, sep=".", drop=T)

cold$month <- cold$delete_1;  cold$delete_1<-cold$delete_2<- NULL
cold$day <- ((cold$Date/100)- round((cold$Date/100),0))*100

cold$date <- paste(round(cold$month,0),round(cold$day,0), cold$year, sep="/")

cold$date <- as.Date(cold$date, '%m/%d/%Y') # formats the date

cold <- cold[,-c(1, 5, 7, 9, 11:14)] # remove columns

names(cold) <-c("Station", "Date",	"tmean",	"tmax",	"tmin", "pp",	"date") # change column names

cold <- cold[which(cold$date > '1949-12-31'),]

cold[cold == -9999] <- NA # change all -9999 to NA
cold.events <- cold[which(cold$tmin <= 5),]

cold.events$count <- 1 # adds a counter to the file

cold.events$year <- year(cold.events$date)

# Total number of events by station:
event.site <- aggregate(count ~ Station, data = cold.events, FUN =sum)
event.site$Mean <- event.site$count/64
event.site$station <-c("EVG", "FLG","RPR","TAV")
events.table <- event.site[, c(4, 2,3)]
events.table[,2]<-round(events.table[,2],0);events.table[,3]<- round(events.table[,3],1)
colnames(events.table) <- c("Station", "Frequency", "Mean Annual Frequency")

# Reorder the Events table:
events.table$order <-c( 1, 3, 2,4) 
rownames(events.table, do.NULL = TRUE)
events.table <- events.table[order(events.table$order),]

setwd("~/git/Cold-Events/figures")
write.csv(events.table,'events_station.csv') # write File for future use!

# Show north to south trends in temperature data 1950 - 2013 for weather station in ENP.
lm.events <- lm(events.table[,3]~events.table[,4])
summary(lm.events)

#__________________________________________________________________________________________________________________
# Figure to show North to South Trend in weather station data:
par(mfrow=c(1,1), tck=0.02, cex=1.2, mai=c(2.5,2.5, 1.5,0.5), cex=2)
plot(events.table[,4],events.table[,3], ylim=c(0, 10), 
     ylab="Cold Events (Mean Annual Frequency)", xaxt='n',
     pch=22, cex=1.5, lwd=2, col="dimgrey",
     xlab="Weather Stations")
axis(1, at=events.table[,4], labels=events.table[,1])
abline(lm.events, col="lightblue", lwd=1.5)

r2.label <- bquote(italic(R)^2 == .(format(summary(lm.events)$r.squared, digits=2)))
p.val<- bquote(italic(p-value) == .(format(summary(lm.events)$coefficients[,4][2] , digits=2)))
# eq <-paste("y = ",summary(lm.events)$coefficients[1]," - ", abs(summary(lm.events)$coefficients[2]),"x", sep="" )
text(3, 9, labels=r2.label)
text(3, 8, labels=p.val)
#__________________________________________________________________________________________________________________

# Linear model showing mean annualfrequency of cold events by weather stations (North to south order)
# counts per year by station:
event.yr.site <- aggregate(count ~ Station + year, data = cold.events, FUN =sum)

#__________________________________________________________________________________________________________________
# Figure showing frequency by year for each station:
par( mfrow=c(4, 1), tck=0.05, mai=c(0.65,1.3,0.25,0.25), cex=1.2)

plot(event.yr.site$year[which(event.yr.site$Station == 'Everglades')],
     event.yr.site$count[which(event.yr.site$Station == 'Everglades')], col="grey",
     ylim=c(0, 25), xlim=c(1950, 2013), pch=15, ylab="EVG", xlab="")
mtext(text="a.", side=3, adj=0.02, cex=1.5, outer=F, line=-1)

plot(event.yr.site$year[which(event.yr.site$Station == 'RoyalPalmR')],
     event.yr.site$count[which(event.yr.site$Station == 'RoyalPalmR')], col="lightblue",
     ylim=c(0, 25), xlim=c(1950, 2013), pch=15, ylab="RPR", xlab="")
mtext(text="b.", side=3, adj=0.02, cex=1.5, outer=F, line=-1)

plot(event.yr.site$year[which(event.yr.site$Station == 'FlamingoRS')],
     event.yr.site$count[which(event.yr.site$Station == 'FlamingoRS')], col="darkblue",
     ylim=c(0, 25), xlim=c(1950, 2013), pch=15, ylab="FLG", xlab="")
mtext(text="c.", side=3, adj=0.02, cex=1.5, outer=F, line=-1)

plot(event.yr.site$year[which(event.yr.site$Station == 'Tavernier')],
     event.yr.site$count[which(event.yr.site$Station == 'Tavernier')], col="blue",
     ylim=c(0, 25), xlim=c(1950, 2013), pch=15, ylab="TAV", xlab="")
mtext(text="d.", side=3, adj=0.02, cex=1.5, outer=F, line=-1)
#__________________________________________________________________________________________________________________
