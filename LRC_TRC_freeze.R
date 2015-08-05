#Sparkle Malone (sparklelmalone@gmail.com)
# fit LRC and TRC for SRS frz events

#Creates a parameter file to store all the coefficients in: 
parameters.frz<- data.frame(
  year=numeric(),
  month=numeric(),
  a1=numeric(),
  ax=numeric(),
  r=numeric(),
  a=numeric(),
  b=numeric(), stringsAsFactors=FALSE, row.names=NULL)

# Create sanFrz files for each yearmon where a freeze event occurs
#2009
SRS.frz.1.2009<-subset(srs, year == 2009 & month == "01" & freeze == 1)
SRS.frz.2.2009<-subset(srs, year == 2009 & month == "02" & freeze == 1)  
#2010
SRS.frz.1.2010<-subset(srs, year == 2010 & month == "01" & freeze == 1)
SRS.frz.2.2010<-subset(srs, year == 2010 & month == "02" & freeze == 1)
SRS.frz.3.2010<-subset(srs, year == 2010 & month == "03" & freeze == 1)
SRS.frz.12.2010<-subset(srs, year == 2010 & month == "12" & freeze == 1)
#2011
SRS.frz.1.2011<-subset(srs, year == 2011 & month == "01" & freeze == 1)
#2012* no freeze events ovbserved at srs

# Models for Frz
#______________________________________2009-01__________________________________________________________________________
#model coefficients not significant due to small sample size!!!
night.SRS.frz.1.2009 = nls(NEEnight ~ (a * exp(b*TA)), 
                              SRS.frz.1.2009, start=list(a=0.4 , b=0.1 ), na.action=na.exclude, trace=F)
day.SRS.frz.1.2009  = nls(NEEDay ~ (a1 *srs.par.filled * ax)/(a1 * srs.par.filled + ax) + r, 
                             SRS.frz.1.2009, start=list(a1=0.001 , ax= 0.1, r= 1), na.action=na.exclude, trace=F)

summary(night.SRS.frz.1.2009)
summary(day.SRS.frz.1.2009)
# Add data to the parms files:
parameters.frz[1,1]<- (2009)
parameters.frz[1,2]<- (1)
parameters.frz[1,3:7] <-rbind(merge(as.data.frame( t(coef(summary(day.SRS.frz.1.2009)) [1:3, 1])), 
                                as.data.frame(t(coef(summary(night.SRS.frz.1.2009)) [1:2, 1])))) 
#______________________________________2009-02__________________________________________________________________________
#model coefficients not significant due to small sample size!!!
night.SRS.frz.2.2009 = nls(NEEnight ~ (a * exp(b*TA)), 
                           SRS.frz.2.2009, start=list(a=0.4 , b=0.01 ), na.action=na.exclude, trace=F)
day.SRS.frz.2.2009  = nls(NEEDay ~ (a1 *srs.par.filled * ax)/(a1 * srs.par.filled + ax) + r, 
                          SRS.frz.2.2009, start=list(a1=0.01 , ax= 1, r= 1), na.action=na.exclude, trace=F)

summary(night.SRS.frz.2.2009)
summary(day.SRS.frz.2.2009)
# Add data to the parms files:
parameters.frz[2,1]<- (2009)
parameters.frz[2,2]<- (2)
parameters.frz[2,3:7] <-rbind(merge(as.data.frame( t(coef(summary(day.SRS.frz.2.2009)) [1:3, 1])), 
                                    as.data.frame(t(coef(summary(night.SRS.frz.2.2009)) [1:2, 1])))) 

#______________________________________2010_____________________________________________________________________________
#______________________________________2010-01__________________________________________________________________________

night.SRS.frz.1.2010 = nls(NEEnight ~ (a * exp(b*TA)), 
                           SRS.frz.1.2010, start=list(a=0.4 , b=0.1 ), na.action=na.exclude, trace=F)
day.SRS.frz.1.2010  = nls(NEEDay ~ (a1 *srs.par.filled * ax)/(a1 * srs.par.filled + ax) + r, 
                          SRS.frz.1.2010, start=list(a1=0.001 , ax= 0.1, r= 1), na.action=na.exclude, trace=F)

summary(night.SRS.frz.1.2010)
summary(day.SRS.frz.1.2010)
# Add data to the parms files:
parameters.frz[3,1]<- (2010)
parameters.frz[3,2]<- (1)
parameters.frz[3,3:7] <-rbind(merge(as.data.frame( t(coef(summary(day.SRS.frz.1.2010)) [1:3, 1])), 
                                    as.data.frame(t(coef(summary(night.SRS.frz.1.2010)) [1:2, 1])))) 

#______________________________________2010-02__________________________________________________________________________

night.SRS.frz.2.2010 = nls(NEEnight ~ (a * exp(b*TA)), 
                           SRS.frz.2.2010, start=list(a=0.4 , b=0.1 ), na.action=na.exclude, trace=F)
day.SRS.frz.2.2010  = nls(NEEDay ~ (a1 *srs.par.filled * ax)/(a1 * srs.par.filled + ax) + r, 
                          SRS.frz.2.2010, start=list(a1=0.001 , ax= 0.1, r= 1), na.action=na.exclude, trace=F)

summary(night.SRS.frz.2.2010)
summary(day.SRS.frz.2.2010)
# Add data to the parms files:
parameters.frz[4,1]<- (2010)
parameters.frz[4,2]<- (2)
parameters.frz[4,3:7] <-rbind(merge(as.data.frame( t(coef(summary(day.SRS.frz.2.2010)) [1:3, 1])), 
                                    as.data.frame(t(coef(summary(night.SRS.frz.2.2010)) [1:2, 1])))) 

#______________________________________2010-03__________________________________________________________________________

night.SRS.frz.3.2010 = nls(NEEnight ~ (a * exp(b*TA)), 
                           SRS.frz.3.2010, start=list(a=0.4 , b=0.1 ), na.action=na.exclude, trace=F)
day.SRS.frz.3.2010  = nls(NEEDay ~ (a1 *srs.par.filled * ax)/(a1 * srs.par.filled + ax) + r, 
                          SRS.frz.3.2010, start=list(a1=0.001 , ax= 0.1, r= 1), na.action=na.exclude, trace=F)

summary(night.SRS.frz.3.2010)
summary(day.SRS.frz.3.2010)
# Add data to the parms files:
parameters.frz[5,1]<- (2010)
parameters.frz[5,2]<- (3)
parameters.frz[5,3:7] <-rbind(merge(as.data.frame( t(coef(summary(day.SRS.frz.3.2010)) [1:3, 1])), 
                                    as.data.frame(t(coef(summary(night.SRS.frz.3.2010)) [1:2, 1])))) 
#______________________________________2010-12__________________________________________________________________________
# some coefficients are not significant!!!
night.SRS.frz.12.2010 = nls(NEEnight ~ (a * exp(b*TA)), 
                           SRS.frz.12.2010, start=list(a=0.4 , b=0.01 ), na.action=na.exclude, trace=F)
day.SRS.frz.12.2010  = nls(NEEDay ~ (a1 *srs.par.filled * ax)/(a1 * srs.par.filled + ax) + r, 
                          SRS.frz.12.2010, start=list(a1=0.001 , ax= 0.1, r= 1), na.action=na.exclude, trace=F)

summary(night.SRS.frz.12.2010)
summary(day.SRS.frz.12.2010)
# Add data to the parms files:
parameters.frz[6,1]<- (2010)
parameters.frz[6,2]<- (12)
parameters.frz[6,3:7] <-rbind(merge(as.data.frame( t(coef(summary(day.SRS.frz.12.2010)) [1:3, 1])), 
                                    as.data.frame(t(coef(summary(night.SRS.frz.12.2010)) [1:2, 1]))))

#______________________________________2011_____________________________________________________________________________
#______________________________________2011-01__________________________________________________________________________
#model coefficients not significant due to small sample size!!!
night.SRS.frz.1.2011 = nls(NEEnight ~ (a * exp(b*TA)), 
                           SRS.frz.1.2011, start=list(a=0.4 , b=0.1 ), na.action=na.exclude, trace=F)
day.SRS.frz.1.2011  = nls(NEEDay ~ (a1 *srs.par.filled * ax)/(a1 * srs.par.filled + ax) + r, 
                          SRS.frz.1.2011, start=list(a1=0.001 , ax= 1, r= 1), na.action=na.exclude, trace=F)

summary(night.SRS.frz.1.2011)
summary(day.SRS.frz.1.2011)
# Add data to the parms files:
parameters.frz[7,1]<- (2011)
parameters.frz[7,2]<- (1)
parameters.frz[7,3:7] <-rbind(merge(as.data.frame( t(coef(summary(day.SRS.frz.1.2011)) [1:3, 1])), 
                                    as.data.frame(t(coef(summary(night.SRS.frz.1.2011)) [1:2, 1])))) 


