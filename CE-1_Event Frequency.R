# Author:
# Date:
# Objective:

library(RCurl)
library(devtools)
install_github("leeper/rio")
library("rio")

# Import data files.

evg <- import('https://github.com/Everglades-Carbon/Cold-Events/raw/master/WeatherStation_Data/FCE_Everglades_ClimDB_data.csv')
fla <- import('https://github.com/Everglades-Carbon/Cold-Events/raw/master/WeatherStation_Data/FCE_FlamingoRS_ClimDB_data.csv')
rp <- import('https://github.com/Everglades-Carbon/Cold-Events/raw/master/WeatherStation_Data/FCE_RoyalPalmRS_ClimDB_data.csv')
tav <- import('https://github.com/Everglades-Carbon/Cold-Events/raw/master/WeatherStation_Data/FCE_Tavernier_ClimDB_data.csv')

cold <- rbind(evg, fla, rp, tav)
rm(evg, fla, rp, tav,ce)

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

cold <- cold[which(cold$date < 01/01/1950),]

cold[cold == -9999] <- NA # change all -9999 to NA
cold.events <- cold[which(cold$tmin <= 5),]

cold.events$count <- 1 # adds a counter to the file

# Total number of events by station:
event.site <- aggregate(count ~ Station, data = cold.events, FUN =sum)

# counts per year by station:
