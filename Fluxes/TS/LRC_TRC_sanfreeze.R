#Sparkle Malone (sparklelmalone@gmail.com)
# fit LRC and TRC for SRS frz vs non Freeze events


#Creates a parameter file to store all the coefficients in: 
parameters<- data.frame(
  year=numeric(),
  month=numeric(),
  a1=numeric(),
  ax=numeric(),
  r=numeric(),
  a=numeric(),
  b=numeric(), stringsAsFactors=FALSE, row.names=NULL)

#creates file for Freeze and non Freeze events:
SRS.sanfrz<-subset(SRS_curves, freeze == 0 )
SRS.frz<-subset(SRS_curves, freeze == 1 )

# Create sanFrz files for each yearmon where a freeze event occurs
#2009
SRS.sanfrz.1.2009<-subset(SRS_curves, year == 2009 & month == "01" & freeze == 0)
SRS.sanfrz.2.2009<-subset(SRS_curves, year == 2009 & month == "02" & freeze == 0)
SRS.sanfrz.3.2009<-subset(SRS_curves, year == 2009 & month == "03" & freeze == 0)
#rm(SRS.sanfrz.1.2009,SRS.sanfrz.2.2009, SRS.sanfrz.3.2009 )
#2010
SRS.sanfrz.1.2010<-subset(SRS_curves, year == 2010 & month == "01" & freeze == 0)
SRS.sanfrz.2.2010<-subset(SRS_curves, year == 2010 & month == "02" & freeze == 0)
SRS.sanfrz.3.2010<-subset(SRS_curves, year == 2010 & month == "03" & freeze == 0)
SRS.sanfrz.12.2010<-subset(SRS_curves, year == 2010 & month == "12" & freeze == 0)
#rm(SRS.sanfrz.1.2010,SRS.sanfrz.2.2010, SRS.sanfrz.3.2010, SRS.sanfrz.12.2010 )
#2011
SRS.sanfrz.1.2011<-subset(SRS_curves, year == 2011 & month == "01" & freeze == 0)
SRS.sanfrz.3.2011<-subset(SRS_curves, year == 2011 & month == "03" & freeze == 0)
#rm(SRS.sanfrz.1.2011, SRS.sanfrz.3.2011 )
#2012
SRS.sanfrz.1.2012<-subset(SRS_curves, year == 2012 & month == "01" & freeze == 0)
SRS.sanfrz.3.2012<-subset(SRS_curves, year == 2012 & month == "03" & freeze == 0)
#rm(SRS.sanfrz.1.2012, SRS.sanfrz.3.2012)


#______________________________________2009-01__________________________________________________________________________
# Models for SanFrz 2009-01
night.SRS.sanfrz.1.2009 = nls(NEEnight ~ (a * exp(b*TA)), 
                              SRS.sanfrz.1.2009, start=list(a=0.4 , b=0.1 ), na.action=na.exclude, trace=F)
day.SRS.sanfrz.1.2009  = nls(NEEDay ~ (a1 *srs.par.filled * ax)/(a1 * srs.par.filled + ax) + r, 
                             SRS.sanfrz.1.2009, start=list(a1=0.1 , ax= 1, r= 1), na.action=na.exclude, trace=F)
# Add data to the parms files:
parameters[1,1]<- (2009)
parameters[1,2]<- (1)
parameters[1,3:7] <-rbind(merge(as.data.frame( t(coef(summary(day.SRS.sanfrz.1.2009)) [1:3, 1])), 
                                as.data.frame(t(coef(summary(night.SRS.sanfrz.1.2009)) [1:2, 1])))) 

#______________________________________2009-02__________________________________________________________________________
night.SRS.sanfrz.2.2009 = nls(NEEnight ~ (a * exp(b*TA)), 
                              SRS.sanfrz.2.2009, start=list(a=0.4 , b=0.1 ), na.action=na.exclude, trace=F)
day.SRS.sanfrz.2.2009  = nls(NEEDay ~ (a1 *srs.par.filled * ax)/(a1 * srs.par.filled + ax) + r, 
                             SRS.sanfrz.2.2009, start=list(a1=0.01 , ax= 1, r= 1), na.action=na.exclude, trace=F)

summary(night.SRS.sanfrz.2.2009)
summary(day.SRS.sanfrz.2.2009)
# Add data to the parms files:
parameters[2,1]<- (2009)
parameters[2,2]<- (2)
parameters[2,3:7] <-rbind(merge(as.data.frame( t(coef(summary(day.SRS.sanfrz.2.2009)) [1:3, 1])), 
                                as.data.frame(t(coef(summary(night.SRS.sanfrz.2.2009)) [1:2, 1])))) 

#______________________________________2009-03__________________________________________________________________________
night.SRS.sanfrz.3.2009 = nls(NEEnight ~ (a * exp(b*TA)), 
                              SRS.sanfrz.3.2009, start=list(a=0.4 , b=0.1 ), na.action=na.exclude, trace=F)
day.SRS.sanfrz.3.2009  = nls(NEEDay ~ (a1 *srs.par.filled * ax)/(a1 * srs.par.filled + ax) + r, 
                             SRS.sanfrz.3.2009, start=list(a1=0.01 , ax= 1, r= 1), na.action=na.exclude, trace=F)

summary(night.SRS.sanfrz.3.2009)
summary(day.SRS.sanfrz.3.2009)
# Add data to the parms files:
parameters[3,1]<- (2009)
parameters[3,2]<- (3)
parameters[3,3:7] <-rbind(merge(as.data.frame( t(coef(summary(day.SRS.sanfrz.3.2009)) [1:3, 1])), 
                                as.data.frame(t(coef(summary(night.SRS.sanfrz.3.2009)) [1:2, 1])))) 

#______________________________________2010_____________________________________________________________________________
#______________________________________2010-01__________________________________________________________________________
night.SRS.sanfrz.1.2010 = nls(NEEnight ~ (a * exp(b*TA)), 
                              SRS.sanfrz.1.2010, start=list(a=0.4 , b=0.1 ), na.action=na.exclude, trace=F)
day.SRS.sanfrz.1.2010  = nls(NEEDay ~ (a1 *srs.par.filled * ax)/(a1 * srs.par.filled + ax) + r, 
                             SRS.sanfrz.1.2010, start=list(a1=0.01 , ax= 1, r= 1), na.action=na.exclude, trace=F)

summary(night.SRS.sanfrz.1.2010)
summary(day.SRS.sanfrz.1.2010)
# Add data to the parms files:
parameters[4,1]<- (2010)
parameters[4,2]<- (1)
parameters[4,3:7] <-rbind(merge(as.data.frame( t(coef(summary(day.SRS.sanfrz.1.2010)) [1:3, 1])), 
                                as.data.frame(t(coef(summary(night.SRS.sanfrz.1.2010)) [1:2, 1])))) 

#______________________________________2010-02__________________________________________________________________________
night.SRS.sanfrz.2.2010 = nls(NEEnight ~ (a * exp(b*TA)), 
                              SRS.sanfrz.2.2010, start=list(a=0.4 , b=0.1 ), na.action=na.exclude, trace=F)
day.SRS.sanfrz.2.2010  = nls(NEEDay ~ (a1 *srs.par.filled * ax)/(a1 * srs.par.filled + ax) + r, 
                             SRS.sanfrz.2.2010, start=list(a1=0.01 , ax= 1, r= 1), na.action=na.exclude, trace=F)

summary(night.SRS.sanfrz.2.2010)
summary(day.SRS.sanfrz.2.2010)
# Add data to the parms files:
parameters[5,1]<- (2010)
parameters[5,2]<- (2)
parameters[5,3:7] <-rbind(merge(as.data.frame( t(coef(summary(day.SRS.sanfrz.2.2010)) [1:3, 1])), 
                                as.data.frame(t(coef(summary(night.SRS.sanfrz.2.2010)) [1:2, 1])))) 

#______________________________________2010-03__________________________________________________________________________
night.SRS.sanfrz.3.2010 = nls(NEEnight ~ (a * exp(b*TA)), 
                              SRS.sanfrz.3.2010, start=list(a=0.4 , b=0.1 ), na.action=na.exclude, trace=F)
day.SRS.sanfrz.3.2010  = nls(NEEDay ~ (a1 *srs.par.filled * ax)/(a1 * srs.par.filled + ax) + r, 
                             SRS.sanfrz.3.2010, start=list(a1=0.01 , ax= 1, r= 1), na.action=na.exclude, trace=F)

summary(night.SRS.sanfrz.3.2010)
summary(day.SRS.sanfrz.3.2010)
# Add data to the parms files:
parameters[6,1]<- (2010)
parameters[6,2]<- (3)
parameters[6,3:7] <-rbind(merge(as.data.frame( t(coef(summary(day.SRS.sanfrz.3.2010)) [1:3, 1])), 
                                as.data.frame(t(coef(summary(night.SRS.sanfrz.3.2010)) [1:2, 1])))) 

#______________________________________2010-12__________________________________________________________________________
night.SRS.sanfrz.12.2010 = nls(NEEnight ~ (a * exp(b*TA)), 
                               SRS.sanfrz.12.2010, start=list(a=0.4 , b=0.1 ), na.action=na.exclude, trace=F)
day.SRS.sanfrz.12.2010  = nls(NEEDay ~ (a1 *srs.par.filled * ax)/(a1 * srs.par.filled + ax) + r, 
                              SRS.sanfrz.12.2010, start=list(a1=0.01 , ax= 1, r= 1), na.action=na.exclude, trace=F)

summary(night.SRS.sanfrz.12.2010)
summary(day.SRS.sanfrz.12.2010)
# Add data to the parms files:
parameters[7,1]<- (2010)
parameters[7,2]<- (12)
parameters[7,3:7] <-rbind(merge(as.data.frame( t(coef(summary(day.SRS.sanfrz.12.2010)) [1:3, 1])), 
                                as.data.frame(t(coef(summary(night.SRS.sanfrz.12.2010)) [1:2, 1])))) 
#______________________________________2011______________________________________________________________________________

#______________________________________2011-01___________________________________________________________________________
night.SRS.sanfrz.1.2011 = nls(NEEnight ~ (a * exp(b*TA)), 
                              SRS.sanfrz.1.2011, start=list(a=0.4 , b=0.1 ), na.action=na.exclude, trace=F)
day.SRS.sanfrz.1.2011  = nls(NEEDay ~ (a1 *srs.par.filled * ax)/(a1 * srs.par.filled + ax) + r, 
                             SRS.sanfrz.1.2011, start=list(a1=0.01 , ax= 0.1, r= 1), na.action=na.exclude, trace=F)

summary(night.SRS.sanfrz.1.2011)
summary(day.SRS.sanfrz.1.2011)
# Add data to the parms files:
parameters[8,1]<- (2011)
parameters[8,2]<- (1)
parameters[8,3:7] <-rbind(merge(as.data.frame( t(coef(summary(day.SRS.sanfrz.1.2011)) [1:3, 1])), 
                                as.data.frame(t(coef(summary(night.SRS.sanfrz.1.2011)) [1:2, 1])))) 
#______________________________________2011-03___________________________________________________________________________
# day equation has non-significant coeficcients!!!!
night.SRS.sanfrz.3.2011 = nls(NEEnight ~ (a * exp(b*TA)), 
                              SRS.sanfrz.3.2011, start=list(a=0.4 , b=0.1 ), na.action=na.exclude, trace=F)
day.SRS.sanfrz.3.2011  = nls(NEEDay ~ (a1 *srs.par.filled * ax)/(a1 * srs.par.filled + ax) + r, 
                             SRS.sanfrz.3.2011, start=list(a1=0.1 , ax= 1, r= 1), na.action=na.exclude, trace=F)

summary(night.SRS.sanfrz.3.2011)
summary(day.SRS.sanfrz.3.2011)
# Add data to the parms files:
parameters[9,1]<- (2011)
parameters[9,2]<- (3)
parameters[9,3:7] <-rbind(merge(as.data.frame( t(coef(summary(day.SRS.sanfrz.3.2011)) [1:3, 1])), 
                                as.data.frame(t(coef(summary(night.SRS.sanfrz.3.2011)) [1:2, 1])))) 

#______________________________________2012______________________________________________________________________________

#______________________________________2012-01___________________________________________________________________________
night.SRS.sanfrz.1.2012 = nls(NEEnight ~ (a * exp(b*TA)), 
                              SRS.sanfrz.1.2012, start=list(a=0.4 , b=0.1 ), na.action=na.exclude, trace=F)
day.SRS.sanfrz.1.2012  = nls(NEEDay ~ (a1 *srs.par.filled * ax)/(a1 * srs.par.filled + ax) + r, 
                             SRS.sanfrz.1.2012, start=list(a1=0.01 , ax= 0.1, r= 1), na.action=na.exclude, trace=F)

summary(night.SRS.sanfrz.1.2012)
summary(day.SRS.sanfrz.1.2012)
# Add data to the parms files:
parameters[10,1]<- (2012)
parameters[10,2]<- (1)
parameters[10,3:7] <-rbind(merge(as.data.frame( t(coef(summary(day.SRS.sanfrz.1.2012)) [1:3, 1])), 
                                 as.data.frame(t(coef(summary(night.SRS.sanfrz.1.2012)) [1:2, 1])))) 

#______________________________________2012-03___________________________________________________________________________
# Coefficient b is not sig!!!
night.SRS.sanfrz.3.2012 = nls(NEEnight ~ (a * exp(b*TA)), 
                              SRS.sanfrz.3.2012, start=list(a=0.4 , b=0.1 ), na.action=na.exclude, trace=F)
day.SRS.sanfrz.3.2012  = nls(NEEDay ~ (a1 *srs.par.filled * ax)/(a1 * srs.par.filled + ax) + r, 
                             SRS.sanfrz.3.2012, start=list(a1=0.01 , ax= 0.1, r= 1), na.action=na.exclude, trace=F)

summary(night.SRS.sanfrz.3.2012)
summary(day.SRS.sanfrz.3.2012)
# Add data to the parms files:
parameters[11,1]<- (2012)
parameters[11,2]<- (3)
parameters[11,3:7] <-rbind(merge(as.data.frame( t(coef(summary(day.SRS.sanfrz.3.2012)) [1:3, 1])), 
                                 as.data.frame(t(coef(summary(night.SRS.sanfrz.3.2012)) [1:2, 1])))) 

