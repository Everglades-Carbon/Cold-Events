# Author:
# Date:
# Objective:

library(RCurl)
library(devtools)
install_github("leeper/rio")
library("rio")

# Import data files.

test <- import('https://github.com/Everglades-Carbon/Cold-Events/raw/master/WeatherStation_Data/FCE_Everglades_ClimDB_data.csv')
attach(test)
test$t1 <-test$Date/10000 
library(splitstackshape)
test<-concat.split(test, 12, sep=".", drop=F)
test$t1 <-(Date-(round(test$Date/10000,0)*10000))/100
test<-concat.split(test, 14, sep=".", drop=F)
