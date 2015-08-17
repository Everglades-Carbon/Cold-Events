#Author: Sparkle Malone
#Date: September 2014
#Objective: Import flux data and format it for cold event Analysis
#Sites: Taylor Slough(ts), Shark River Slough (srs), Mangroves (ms)

rm(list=ls())

#Import all srs data:

setwd("~/git/Cold-Events/Fluxes/SRS")
file_list_SRS<-list.files(pattern="*SRS.csv")
print(file_list_SRS)
for (file_srs in file_list_SRS){
  if (!exists("srs")){
    srs <- read.csv(file_srs, header=T,, stringsAsFactors=FALSE,colClasses="numeric")
     }
  if (exists("srs")){
    temp_dataset_srs <-read.csv(file_srs, header=T, stringsAsFactors=FALSE,colClasses="numeric")
    srs<-rbind(srs, temp_dataset_srs)
    rm(temp_dataset_srs)
  }
}

names(srs)<-c('YEAR','GAP','DTIME','DOY','HRMIN','UST','TA','WD','WS','NEE','FC','SFC','H','SH','LE','SLE',
              'FG','TS1','TSdepth1','TS2','TSdepth2','PREC','RH','PRESS','CO2',  'VPD','SWC1','SWC2','Rn','PAR',  
              'Rg','Rgdif','PARout','RgOut','Rgl','RglOut','H2O','RE','GPP','CO2top','CO2height','APAR','PARdif','APARpct','ZL')

rm(file_list_SRS, file_srs)

#Import all ts data:*will not import ts with loop?
setwd("~/git/Cold-Events/Fluxes/TS")

ts_a <- read.csv("~/git/Cold-Events/Fluxes/TS/AMF_USEsm_2008_L2_GF_V004_TS.csv", 
                 header=T, stringsAsFactors=FALSE,colClasses="numeric")
ts_b <- read.csv("~/git/Cold-Events/Fluxes/TS/AMF_USEsm_2009_L2_GF_V004_TS.csv", 
                 header=T, stringsAsFactors=FALSE,colClasses="numeric")
ts_c <- read.csv("~/git/Cold-Events/Fluxes/TS/AMF_USEsm_2010_L2_GF_V004_TS.csv", 
                 header=T, stringsAsFactors=FALSE,colClasses="numeric")
ts_d <- read.csv("~/git/Cold-Events/Fluxes/TS/AMF_USEsm_2011_L2_GF_V004_TS.csv", 
                 header=T, stringsAsFactors=FALSE,colClasses="numeric")
ts_e <- read.csv("~/git/Cold-Events/Fluxes/TS/AMF_USEsm_2012_L2_GF_V004_TS.csv", 
                 header=T, stringsAsFactors=FALSE,colClasses="numeric")

ts<-rbind(ts_a, ts_b, ts_c, ts_d, ts_e)
names(ts)<-c('YEAR','GAP','DTIME','DOY','HRMIN','UST','TA','WD','WS','NEE','FC','SFC','H','SH','LE','SLE',
             'FG','TS1','TSdepth1','TS2','TSdepth2','PREC','RH','PRESS','CO2',  'VPD','SWC1','SWC2','Rn','PAR',
             'Rg','Rgdif','PARout','RgOut','Rgl','RglOut','H2O','RE','GPP','CO2top','CO2height','APAR','PARdif','APARpct','ZL')
rm(ts_a, ts_b, ts_c, ts_d, ts_e)


#Imports Mangroves(ms)data

ms<-read.csv("~/git/Cold-Events/Fluxes/Man/Barr_data.csv", header=T)

#subset Ameriflux datasets
srs_data<-as.data.frame(cbind(srs$YEAR,srs$DTIME, srs$DOY,srs$HRMIN,srs$TA, srs$NEE, srs$GPP, srs$PAR))
names(srs_data)<-c('YEAR', 'DTIME', 'DOY', 'HRMIN','TA','NEE','GPP','PAR')

ts_data<-as.data.frame(cbind(ts$YEAR,ts$DTIME, ts$DOY,ts$HRMIN,ts$TA, ts$NEE, ts$GPP, ts$PAR))
names(ts_data)<-c('YEAR', 'DTIME', 'DOY', 'HRMIN','TA','NEE','GPP','PAR')

rm(srs, ts)

#create month from DOY:
Month4DOY<-function(a,c){
  y<-1
  if (a == 2008 | a ==2012){
    y[ c <= 31]<-1
    y[ c >= 32 & c <= 60 ]<- 2
    y[ c >= 61 & c <= 91 ]<- 3
    y[ c >= 92 & c <= 121 ]<- 4
    y[ c >= 122 & c <= 152 ]<- 5
    y[ c >= 153 & c <= 182 ]<- 6
    y[ c >= 183 & c <= 213 ]<- 7
    y[ c >= 214 & c <= 244 ]<- 8
    y[ c >= 245 & c <= 274 ]<- 9
    y[ c >= 275 & c <= 305 ]<- 10
    y[ c >= 306 & c <= 335 ]<- 11
    y[ c >= 336 & c <= 366 ]<- 12 
  }else{
    if (a != 2008 | a !=2012){
      y[ c <= 31]<-1
      y[ c >= 32 & c <= 59 ]<- 2
      y[ c >= 60 & c <= 90 ]<- 3
      y[ c >= 91 & c <= 120 ]<- 4
      y[ c >= 121 & c <= 151 ]<- 5
      y[ c >= 152 & c <= 181 ]<- 6
      y[ c >= 182 & c <= 212 ]<- 7
      y[ c >= 213 & c <= 243 ]<- 8
      y[ c >= 244 & c <= 273 ]<- 9
      y[ c >= 274 & c <= 304 ]<- 10
      y[ c >= 305 & c <= 334 ]<- 11
      y[ c >= 335 & c <= 365 ]<- 12 }}
  return(y)
}
#create day from DOY:
DAY4DOY<-function(a,c){
  y<-1# must define Y outside the function
  if (a == 2008 | a ==2012){
    y[ c <= 31]<-c
    y[ c >= 32 & c <= 60 ]<- c-31
    y[ c >= 61 & c <= 91 ]<- c-60
    y[ c >= 92 & c <= 121 ]<- c-91
    y[ c >= 122 & c <= 152 ]<- c-121
    y[ c >= 153 & c <= 182 ]<- c-152
    y[ c >= 183 & c <= 213 ]<- c-182
    y[ c >= 214 & c <= 244 ]<- c-213
    y[ c >= 245 & c <= 274 ]<- c-244
    y[ c >= 275 & c <= 305 ]<- c-274
    y[ c >= 306 & c <= 335 ]<- c-305
    y[ c >= 336 & c <= 366 ]<- c-335
  }else{
    if (a != 2008 | a !=2012){
      y[ c <= 31]<-c
      y[ c >= 32 & c <= 59 ]<- c-31
      y[ c >= 60 & c <= 90 ]<- c-59
      y[ c >= 91 & c <= 120 ]<- c-90
      y[ c >= 121 & c <= 151 ]<- c-120
      y[ c >= 152 & c <= 181 ]<- c-151
      y[ c >= 182 & c <= 212 ]<- c-181
      y[ c >= 213 & c <= 243 ]<- c-212
      y[ c >= 244 & c <= 273 ]<- c-243
      y[ c >= 274 & c <= 304 ]<- c-273
      y[ c >= 305 & c <= 334 ]<- c-304
      y[ c >= 335 & c <= 365 ]<- c-334}}
  return(y)
}

#apply the functions (mapply())
ts_data$month<-mapply(Month4DOY,ts_data$YEAR, ts_data$DOY)
ts_data$day<-mapply(DAY4DOY,ts_data$YEAR, ts_data$DOY)
srs_data$month<-mapply(Month4DOY,srs_data$YEAR, srs_data$DOY)
srs_data$day<-mapply(DAY4DOY,srs_data$YEAR, srs_data$DOY)

#separate time into hours and minutes
library("splitstackshape")
ms<-concat.split(ms,4, sep =".", drop = FALSE)
ms$hour_f<-(ms$Time_1)
ms$minutes<-(ms$Time_2)
ms$Time_1<-ms$Time_2<-NULL

#remove all NAs in the minutes field
ms$minutes[is.na(ms$minutes)]<-0

#make all 5s equal to 30 (the half hour)
Minutes<-function(x){
  if (x>0){
    y=30
  }else{y=0}
  return(y)
}
ms$minutes2<-lapply(ms$minutes, Minutes)

#creates a new time feature (hour: miutes)
ms$minutes<-ms$minutes2
ms$hour<-ms$hour_f
ms$hour_f<-ms$minutes2<-NULL

ms$time<-paste(ms$hour,ms$minutes, sep=":")

#format Ameriflux time:
ts_data$time<-ts_data$HRMIN/100
ts_data<-concat.split(ts_data,11, sep =".", drop = FALSE)
ts_data$hour<-(ts_data$time_1)
ts_data$minutes<-(ts_data$time_2)
ts_data$time_1<-ts_data$time_2<-NULL
ts_data$minutes[is.na(ts_data$minutes)]<-0
ts_data$minutes<-ts_data$minutes*10
ts_data$time<-paste(ts_data$hour, ts_data$minutes, sep=":")

srs_data$time<-srs_data$HRMIN/100
srs_data<-concat.split(srs_data,11, sep =".", drop = FALSE)
srs_data$hour<-(srs_data$time_1)
srs_data$minutes<-(srs_data$time_2)
srs_data$time_1<-srs_data$time_2<-NULL
srs_data$minutes[is.na(srs_data$minutes)]<-0
srs_data$minutes<-srs_data$minutes*10
srs_data$time<-paste(srs_data$hour, srs_data$minutes, sep=":")

#creates the date as an date objectand date time objects:
ms$date<-as.Date(ISOdate(ms$Year, ms$Month, ms$Day))
class(ms$date)
ms$datetime<-as.POSIXlt(paste(ms$date,ms$time), "MST7MDT",
                        format= "%Y-%m-%d %H:%M")
class(ms$datetime)
ts_data$date<-as.Date(ISOdate(ts_data$YEAR, ts_data$month, ts_data$day))
class(ts_data$date)
ts_data$datetime<-as.POSIXlt(paste(ts_data$date,ts_data$time), "MST7MDT",
                             format= "%Y-%m-%d %H:%M")
class(ts_data$datetime)
srs_data$date<-as.Date(ISOdate(srs_data$YEAR, srs_data$month, srs_data$day))
class(srs_data$date)
srs_data$datetime<-as.POSIXlt(paste(srs_data$date,srs_data$time), "MST7MDT",
                              format= "%Y-%m-%d %H:%M")
class(srs_data$datetime)

#Remove all duplicate values:
Test_duplicates<-srs_data[duplicated(srs_data$datetime)|duplicated(srs_data$datetime, fromLast=TRUE),] #to see a list of duplicates:
srs_data<-srs_data[!duplicated(srs_data$datetime),] #remove duplicate rows based on time stamp

Test_duplicates<-ts_data[duplicated(ts_data$datetime)|duplicated(ts_data$datetime, fromLast=TRUE),] #to see a list of duplicates:
ts_data<-ts_data[!duplicated(ts_data$datetime),] 
rm(Test_duplicates)

#Change all -9999 in GPP to 0
ts_data$GPP[ts_data$GPP == -9999]<-0
srs_data$GPP[ srs_data$GPP == -9999]<-0
#Change all -9999 in PAR to NA
srs_data$PAR[srs_data$PAR == -9999]<-NA
ts_data$PAR[ts_data$PAR == -9999]<-NA

#subset the dataframe to start at 2009
srs.data<-subset(srs_data, datetime > "2008-12-31 23:30:00" )
ts.data<-subset(ts_data, datetime > "2008-12-31 23:30:00" )
ms.data<-subset(ms, datetime > "2008-12-31 23:30:00" )

#Make the dataframe a timeseries
#install.packages("zoo")
library("zoo")
srs_zoo<-zoo(srs_data[5:8], srs_data$datetime)
ts_zoo<-zoo(ts_data[,5:8], ts_data$datetime)
ms_zoo <-zoo(ms[,5:7],ms$datetime)
rm(srs_data, ts_data, ms, ms.data, ts.data, srs.data)

# Gapfill PAR
PAR_fill_zoo<-merge(srs_zoo$PAR, ts_zoo$PAR,ms_zoo$PAR, all = TRUE, fill = NA, retclass ='zoo')
names(PAR_fill_zoo)<-c("srs", 'ts', 'ms')

# Creates a function to fill missing data in a, with b (other site), or c (jordans pressure data)
Fill <- function(a,b,c){
  if (is.na(a) == T & is.na(b) == T) {
    a=c
  }else{
    if (is.na(a) == T & is.na(b) == F){
      a=b
    }
  }
  return (a)
}


  # Applies function to fill TS and SRS data
  PAR_fill_zoo$srs<-mapply(Fill,PAR_fill_zoo$srs, PAR_fill_zoo$ts,PAR_fill_zoo$ms)
  PAR_fill_zoo$ts<-mapply(Fill,PAR_fill_zoo$ts, PAR_fill_zoo$srs,PAR_fill_zoo$ms)
  PAR_fill_zoo$ms<-mapply(Fill,PAR_fill_zoo$ms, PAR_fill_zoo$ts,PAR_fill_zoo$srs)

#there are just a few missing NA values (fill with na.approx())
srs.par.filled<-na.approx(PAR_fill_zoo$srs, maxgap=10)
ts.par.filled<-na.approx(PAR_fill_zoo$ts, maxgap=10)
ms.par.filled<-na.approx(PAR_fill_zoo$ms, maxgap=10)

# Combines filled PAR with zoo df
srs_zoo<-merge(srs_zoo, srs.par.filled,all = F)
ts_zoo<-merge(ts_zoo, ts.par.filled,all = F)
ms_zoo<-merge(ms_zoo, ms.par.filled,all = F)
rm(srs.par.filled,ts.par.filled, PAR_fill_zoo, ms.par.filled)

# Get RECO in AMeriflux datasets: (a=NEE, B=GPP)
Reco<-function(a, b){
  y= (a-b)
}
ts_zoo$RECO<-(mapply(Reco, ts_zoo$NEE,ts_zoo$GPP))
srs_zoo$RECO<-(mapply(Reco, srs_zoo$NEE,srs_zoo$GPP))
#__________________________________________________________________________________________________#

# Create dataframes
ms_curves<-as.data.frame(ms_zoo)
SRS_curves<-as.data.frame(srs_zoo)
TS_curves<-as.data.frame(ts_zoo)
rm(srs_zoo, ts_zoo, ms_zoo)

#DAYNIGHT indicator
DayNight<-function(a){
  if(a == 0){
    y="night"
  }else{
    y="day"
  }
  return(y)
}

SRS_curves$dayNight<-mapply(DayNight,SRS_curves$srs.par.filled)
TS_curves$dayNight<-mapply(DayNight,TS_curves$ts.par.filled)
ms_curves$dayNight<-mapply(DayNight,ms_curves$ms.par.filled)

# Creates day and night NEE
NEEday<- function(x, z){
  if(x == "day"){
    y = z
  } else{
    y = NA
    return(y)
    }
  }
NEEnight<- function(x, z){
  if(x == "night"){
    y = z
  } else{
    y=NA
  }
  return(y)
}

SRS_curves$NEEDay<-mapply(NEEday, SRS_curves$dayNight, SRS_curves$NEE)
SRS_curves$NEEnight<-mapply(NEEnight, SRS_curves$dayNight, SRS_curves$NEE)

TS_curves$NEEDay<-mapply(NEEday, TS_curves$dayNight, TS_curves$NEE)
TS_curves$NEEnight<-mapply(NEEnight, TS_curves$dayNight, TS_curves$NEE)

ms_curves$NEEDay<-mapply(NEEday, ms_curves$dayNight, ms_curves$NEE)
ms_curves$NEEnight<-mapply(NEEnight, ms_curves$dayNight, ms_curves$NEE)

# Change row names into a column
SRS_curves$datetime <- as.POSIXlt(rownames(SRS_curves), "GMT")
TS_curves$datetime <- as.POSIXlt(rownames(TS_curves), "GMT")
ms_curves$datetime <- as.POSIXlt(rownames(ms_curves), "GMT")

# Create Freeze Indicatior

# create date to aggregate by
SRS_curves$date <- as.Date(format(SRS_curves$datetime, "%Y/%m/%d"))
TS_curves$date <- as.Date(format(TS_curves$datetime, "%Y/%m/%d"))
ms_curves$date <- as.Date(format(ms_curves$datetime, "%Y/%m/%d"))

# Aggregate Tmin by date
library(plyr)
SRS.day <- ddply(SRS_curves,.(date), summarize, date = date[which.min(TA)], TA.min.srs=min(TA))
TS.day <- ddply(TS_curves,.(date), summarize, date = date[which.min(TA)], TA.min.ts=min(TA))
ms.day <- ddply(ms_curves,.(date), summarize, date = date[which.min(TA)], TA.min.ms=min(TA))

freeze<- function (x){
  if (x <= 5){
    y=1}else{y=0
    }
  }
SRS.day$freeze.srs <- sapply(SRS.day$TA, freeze)
TS.day$freeze.ts <- sapply(TS.day$TA, freeze)
ms.day$freeze.ms <- sapply(ms.day$TA, freeze)

srs.f<-SRS.day
srs.f$freeze <- srs.f$freeze.srs
srs.f$TA.min.srs<-srs.f$freeze.srs <- NULL

ts.f<-TS.day
ts.f$freeze <- ts.f$freeze.ts
ts.f$TA.min.ts<-ts.f$freeze.ts <- NULL

ms.f<-ms.day
ms.f$freeze <- ms.f$freeze.ms
ms.f$TA.min.ms<-ms.f$freeze.ms <- NULL

# adds indicator to file
SRS_curves <- merge(SRS_curves, srs.f, by="date", all=T)
TS_curves <- merge(TS_curves, ts.f, by="date", all=T)
ms_curves <- merge(ms_curves, ms.f, by="date", all=T)

# adds year month day to the file
SRS_curves$year <- format(SRS_curves$datetime, "%Y")
SRS_curves$month <- format(SRS_curves$datetime, "%m")
SRS_curves$day <- format(SRS_curves$datetime, "%d")

TS_curves$year <- format(TS_curves$datetime, "%Y")
TS_curves$month <- format(TS_curves$datetime, "%m")
TS_curves$day <- format(TS_curves$datetime, "%d")

ms_curves$year <- format(ms_curves$datetime, "%Y")
ms_curves$month <- format(ms_curves$datetime, "%m")
ms_curves$day <- format(ms_curves$datetime, "%d")


setwd("/Volumes/Sparkle Malone/Projects/Freeze/R/Fluxes")
write.csv(SRS_curves, "SRS_freeze.csv" )
write.csv(TS_curves, "TS_freeze.csv" )
write.csv(ms_curves, "Mangroves_freeze.csv" )