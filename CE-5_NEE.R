# Author: Sparkle L. Malone
# Date: August 2015
# Objective: Fit ARIMA models to NEE and test the effects of freeze events on NEE

rm(list=ls())

#Import datafiles and compile table of interest
setwd("~/git/Cold-Events/Fluxes")

srs <- read.csv("SRS_freeze.csv")
ts <- read.csv("TS_freeze.csv")
ms <- read.csv("Mangroves_freeze.csv")

# Subset the dataframes:
srs <- srs[, c(12, 4, 13, 3)]
ts <- ts[, c(12, 4, 13, 3)]
ms <- ms[, c(10, 5, 11, 4)]

# Format the time:
srs$datetime <- as.POSIXlt(srs$datetime,  "MST7MDT",format= "%Y-%m-%d %H:%M")
ts$datetime <- as.POSIXlt(ts$datetime,  "MST7MDT",format= "%Y-%m-%d %H:%M")
ms$datetime <- as.POSIXlt(ms$datetime,  "MST7MDT",format= "%Y-%m-%d %H:%M")

# Subset dataframes by time:
srs <- srs[which( srs$datetime >= as.POSIXlt("2009-01-01 00:00:00")),]
ts <- ts[which( ts$datetime >= as.POSIXlt("2009-01-01 00:00:00")),]
ms <- ms[which( ms$datetime >= as.POSIXlt("2009-01-01 00:00:00")),]

srs <- srs[which( srs$datetime <= as.POSIXlt("2012-12-31 23:30:00")),]
ts <- ts[which( ts$datetime <= as.POSIXlt("2012-12-31 23:30:00")),]
ms <- ms[which( ms$datetime <=as.POSIXlt("2012-12-31 23:30:00")),]

# Create a date to aggregate by
srs$date <- format(srs$datetime, "%Y-%m-%d")
ts$date <- format(ts$datetime, "%Y-%m-%d")
ms$date <- format(ms$datetime, "%Y-%m-%d")

# calculate NEE from umol m-2 s-1 to g CO2 half hour-1
srs$nee <- srs$NEE * 1800 *(1/1000000)* 44
ts$nee <- ts$NEE * 1800 *(1/1000000)* 44
ms$nee <- ms$NEE * 1800 *(1/1000000)* 44

#aggregate NEE by the day
srs.day <- aggregate( srs$nee , by= list(srs$date), FUN= sum); names(srs.day) <- c("date", "srs.nee") 
srs.frz <- aggregate( srs$freeze , by= list(srs$date), FUN= max); names(srs.frz) <- c("date", "srs.freeze")
srs.ts <- merge(srs.day, srs.frz); rm(srs.day, srs.frz)
srs.ts$date <-as.Date(srs.ts$date)

ts.day <- aggregate( ts$nee , by= list(ts$date), FUN= sum); names(ts.day) <- c("date", "ts.nee") 
ts.frz <- aggregate( ts$freeze , by= list(ts$date), FUN= max); names(ts.frz) <- c("date", "ts.freeze")
ts.ts <- merge(ts.day, ts.frz); rm(ts.day, ts.frz)
ts.ts$date <-as.Date(ts.ts$date)

ms.day <- aggregate( ms$nee , by= list(ms$date), FUN= sum); names(ms.day) <- c("date", "ms.nee") 
ms.frz <- aggregate( ms$freeze , by= list(ms$date), FUN= max); names(ms.frz) <- c("date", "ms.freeze")
ms.ts <- merge(ms.day, ms.frz); rm(ms.day, ms.frz)
ms.ts$date <-as.Date(ms.ts$date)
# Plot of Daily NEE
par(mfrow=c(3,1))
plot(srs.ts$srs.nee, type="l", ylim=c(-25, 10))
plot(ts.ts$ts.nee, type="l", col="grey", ylim=c(-25, 10))
plot(ms.ts$ms.nee, type="l", col="blue", ylim=c(-25, 10))

#Histograms:
par(mfrow=c(3,1))
hist(srs.ts$srs.nee)
hist(ts.ts$ts.nee)
hist(ms.ts$ms.nee)

# QQnorm:
qqnorm(srs.ts$srs.nee); abline(0,1)
qqnorm(ts.ts$ts.nee); abline(0,1)
qqnorm(ms.ts$ms.nee); abline(0,1)

# Normality:
shapiro.test(srs.ts$srs.nee) # not normally distributed!
shapiro.test(ts.ts$ts.nee) # not normally distributed!
shapiro.test(ms.ts$ms.nee) # not normally distributed!
#________________________________________________Unecessary but Interesting_____________________________________
# TimeSeries Objects are used to decompose trends:
nee.srs <- ts(srs.ts$srs.nee[1:1461], start=c(2009, 01, 01), end= c(2013, 01, 01),frequency=365.25)

# Exploring filters:
nee.srs.12 <- filter(nee.srs, filter=rep(1/25, 25))
nee.srs.150<- filter(nee.srs, filter=rep(1/150, 150))

plot(nee.srs , typ="l")
lines(nee.srs.12, col = "red")
lines(nee.srs.150, col = "blue")

# Decompose time series:
library("TTR")
d.test <- decompose(nee.srs); plot(d.test)
#_______________________________________________________________________________________________________________

# Examines autocorrelations:
par(mfrow=c(2,1))
acf(srs.ts$srs.nee, main="ACF of AR process")
pacf(srs.ts$srs.nee, main="PACF of AR process")

# Estimates the best model order:

#SRS:
auto.arima(srs.ts$srs.nee,stationary=T, seasonal=T,ic=c("aicc", "aic", "bic"), stepwise=TRUE, trace=T,
           test=c("kpss","adf","pp"), seasonal.test=c("ocsb","ch"),
           allowdrift=TRUE, allowmean=TRUE, lambda=NULL, parallel=FALSE)

arima.nee.srs.null  <- arima(srs.ts$srs.nee, order=c(2,0,1),method = "ML")
arima.nee.srs  <- arima(srs.ts$srs.nee, order=c(2,0,1), xreg=srs.ts$srs.freeze,method = "ML")

(1-pnorm(abs(arima.nee.srs$coef)/sqrt(diag(arima.nee.srs$var.coef))))*2 # gives the p-vaules

#TS: 
auto.arima(ts.ts$ts.nee,stationary=T, seasonal=T,ic=c("aicc", "aic", "bic"), stepwise=TRUE, trace=T,
           test=c("kpss","adf","pp"), seasonal.test=c("ocsb","ch"),
           allowdrift=TRUE, allowmean=TRUE, lambda=NULL, parallel=FALSE)

arima.nee.ts.null  <- arima(ts.ts$ts.nee, order=c(2,0,1), method = "ML")
arima.nee.ts  <- arima(ts.ts$ts.nee, order=c(2,0,1), xreg=ts.ts$ts.freeze,method = "ML")

(1-pnorm(abs(arima.nee.ts$coef)/sqrt(diag(arima.nee.ts$var.coef))))*2 # gives the p-vaules

#MS: 
auto.arima(ms.ts$ms.nee,stationary=T, seasonal=T,ic=c("aicc", "aic", "bic"), stepwise=TRUE, trace=T,
           test=c("kpss","adf","pp"), seasonal.test=c("ocsb","ch"),
           allowdrift=TRUE, allowmean=TRUE, lambda=NULL, parallel=FALSE)

arima.nee.ms.null  <- arima(ms.ts$ms.nee, order=c(2,0,1),method = c("CSS-ML"))
arima.nee.ms  <- arima(ms.ts$ms.nee, order=c(2,0,1), xreg=ms.ts$ms.freeze, method = c("CSS-ML"))

(1-pnorm(abs(arima.nee.ms$coef)/sqrt(diag(arima.nee.ms$var.coef))))*2 # gives the p-vaules

# Save ARIMA Model DATA in tables:
# SRS
srs.arima.df <- as.data.frame(arima.nee.srs$coef)
srs.p <-as.data.frame((1-pnorm(abs(arima.nee.srs$coef)/sqrt(diag(arima.nee.srs$var.coef))))*2)
srs.se <- as.data.frame(sqrt(diag(arima.nee.srs$var.coef)))
srs.arima.df<- cbind(srs.arima.df , srs.p, match(rownames(srs.arima.df), rownames(srs.p)) )
srs.arima.df <- cbind(srs.arima.df ,srs.se, match(rownames(srs.arima.df), rownames(srs.se)))

srs.arima.df <- srs.arima.df[, -c(3,5)];
srs.arima.df$Parameters <- rownames(srs.arima.df)
srs.arima.df <- srs.arima.df[, c(4,1,3,2)]
srs.arima.df[, c(2:4)] <- round(srs.arima.df[, c(2:4)], 3)
names(srs.arima.df) <- c('Parameter', 'Coefficents', 'S.E', 'p-value')
srs.arima.df[5, 1] <- "Cold Events" ; rownames(srs.arima.df) <-NULL

# TS:
ts.arima.df <- as.data.frame(arima.nee.ts$coef)
ts.p <- as.data.frame((1-pnorm(abs(arima.nee.ts$coef)/sqrt(diag(arima.nee.ts$var.coef))))*2)
ts.se <- as.data.frame(sqrt(diag(arima.nee.ts$var.coef)))
ts.arima.df<- cbind(ts.arima.df , ts.p, match(rownames(ts.arima.df), rownames(ts.p)) )
ts.arima.df <- cbind(ts.arima.df ,ts.se, match(rownames(ts.arima.df), rownames(ts.se)))

ts.arima.df <- ts.arima.df[, -c(3,5)];
ts.arima.df$Parameters <- rownames(ts.arima.df)
ts.arima.df <- ts.arima.df[, c(4,1,3,2)]
ts.arima.df[, c(2:4)] <- round(ts.arima.df[, c(2:4)], 3)
names(ts.arima.df) <- c('Parameter', 'Coefficents', 'S.E', 'p-value')
ts.arima.df[5, 1] <- "Cold Events" ; rownames(ts.arima.df) <-NULL

# MS (Mangroves aka ts-6):
ms.arima.df <- as.data.frame(arima.nee.ms$coef)
ms.p <- as.data.frame((1-pnorm(abs(arima.nee.ms$coef)/sqrt(diag(arima.nee.ms$var.coef))))*2)
ms.se <- as.data.frame(sqrt(diag(arima.nee.ms$var.coef)))
ms.arima.df <- cbind(ms.arima.df , ms.p, match(rownames(ms.arima.df), rownames(ms.p)) )
ms.arima.df <- cbind(ms.arima.df ,ms.se, match(rownames(ms.arima.df), rownames(ms.se)))

ms.arima.df <- ms.arima.df[, -c(3,5)];
ms.arima.df$Parameters <- rownames(ms.arima.df)
ms.arima.df <- ms.arima.df[, c(4,1,3,2)]
ms.arima.df[, c(2:4)] <- round(ms.arima.df[, c(2:4)], 3)
names(ms.arima.df) <- c('Parameter', 'Coefficents', 'S.E', 'p-value')
ms.arima.df[5, 1] <- "Cold Events" ; rownames(ms.arima.df) <-NULL

#Export ARIMA Results:
write.csv(srs.arima.df, "~/git/Cold-Events/Fluxes/arima_results_srs.csv")
write.csv(ts.arima.df, "~/git/Cold-Events/Fluxes/arima_results_ts.csv")
write.csv(ms.arima.df, "~/git/Cold-Events/Fluxes/arima_results_ms.csv")

# Diagnostic Checking:
tsdiag(arima.nee.srs)
tsdiag(arima.nee.ts)
tsdiag(arima.nee.ms)

# It produces following output containing a plot of the residuals, 
## the autocorre- lation of the residuals and the p-values of the Ljung–Box statistic for the first 10 lags

Box.test(arima.nee.srs$residuals,lag=1)
Box.test(arima.nee.ts$residuals,lag=1)
Box.test(arima.nee.ms$residuals,lag=1)

# Predicted values and fitted values of the model:

srs.fitted <- fitted(arima(srs.ts$srs.nee, order=c(2,0,1), xreg=srs.ts$srs.freeze,method = "ML"))
ts.fitted <- fitted(arima(ts.ts$ts.nee, order=c(2,0,1), xreg=ts.ts$ts.freeze,method = "ML"))
ms.fitted <- fitted(arima(ms.ts$ms.nee, order=c(2,0,1), xreg=ms.ts$ms.freeze, method = c("CSS-ML")))

#_________________________________________ ARIMA FIGURE NEE ____________________________________________
par(mfrow=c(3,1), mai=c(1, 1.5, 0.25, 0.25), tck=0.02, cex=1.1)
plot(srs.ts$date, srs.ts$srs.nee, typ="l", xlab="", ylab=expression(paste("NEE ( g CO"[2]," m"^"-2","s"^"-1",")")), 
     ylim=c(-7, 7))
lines(srs.ts$date, srs.fitted, col="blue", typ="l")
abline(v=as.Date('2009-01-01'), lty=2)
abline(v=as.Date('2009-02-01'), lty=2)
abline(v=as.Date('2010-01-01'), lty=2)
abline(v=as.Date('2010-02-01'), lty=2)
abline(v=as.Date('2010-03-01'), lty=2)
abline(v=as.Date('2010-12-01'), lty=2)
abline(v=as.Date('2011-01-01'), lty=2)
abline(v=as.Date('2012-01-01'), lty=2)
mtext(text="a.", side=3, adj=0.01, cex=1.1, outer=F, line=-1)
abline(h=0, col="grey")
plot(ts.ts$date,ts.ts$ts.nee,typ="l", xlab="", ylab=expression(paste("NEE ( g CO"[2]," m"^"-2","s"^"-1",")")),ylim=c(-7, 7))
lines(ts.ts$date,ts.fitted, col="blue", typ="l")
abline(v=as.Date('2009-01-01'), lty=2)
abline(v=as.Date('2009-02-01'), lty=2)
abline(v=as.Date('2009-03-01'), lty=2)
abline(v=as.Date('2010-01-01'), lty=2)
abline(v=as.Date('2010-02-01'), lty=2)
abline(v=as.Date('2010-03-01'), lty=2)
abline(v=as.Date('2010-12-01'), lty=2)
abline(v=as.Date('2011-01-01'), lty=2)
abline(v=as.Date('2011-03-01'), lty=2)
abline(v=as.Date('2012-01-01'), lty=2)
abline(v=as.Date('2012-03-01'), lty=2)
abline(h=0, col="grey")
mtext(text="b.", side=3, adj=0.01, cex=1.1, outer=F, line=-1)
plot(ms.ts$date,ms.ts$ms.nee, typ="l", xlab="", ylab=expression(paste("NEE ( g CO"[2]," m"^"-2","s"^"-1",")")))
lines(ms.ts$date,ms.fitted, col="blue", typ="l")
abline(h=0, col="grey")
abline(v=as.Date('2010-01-01'), lty=2)
abline(v=as.Date('2010-12-01'), lty=2)
abline(v=as.Date('2012-01-01'), lty=2)
mtext(text="c.", side=3, adj=0.01, cex=1.1, outer=F, line=-1)
