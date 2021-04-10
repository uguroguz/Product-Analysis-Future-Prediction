#Load Libraries
install.packages("fUnitRoots")
install.packages("lmtest")
install.packages("FitAR")
library("fUnitRoots")
library(lmtest)
library("forecast")
library(FitAR)
library(ggplot2)

path_in_filename = "wtGroup.csv"
VuSales<-read.csv(path_in_filename)

#creating timeseries
rownames(VuSales)<-VuSales$AyYil
NetSatis.timeseries<- ts(VuSales[,3], frequency = 12, start= c(2019,1))

autoplot(NetSatis.timeseries)

#decompose into time series components
timeSeriesComponents<-decompose(NetSatis.timeseries)
plot(timeSeriesComponents)

#determine stationarity of data
urkpssTest(NetSatis.timeseries, type= c("tau"), lags= "short", use.lag = NULL, doplot= TRUE)
tsStationary<- diff(NetSatis.timeseries, difference = 1)
plot(tsStationary)
acf(NetSatis.timeseries, lag.max = 12)

#Removing Seasonality
timeSeries_seasonal_Adjusted<- NetSatis.timeseries - timeSeriesComponents$seasonal
plot(timeSeries_seasonal_Adjusted)

tsStationary<-diff(timeSeries_seasonal_Adjusted, difference= 1)
plot(tsStationary)

par(mfrow = c(2,1))
acf(tsStationary, lag.max = 12)
pacf(tsStationary, lag.max = 12)


#fit the model
#ACf-> MA(1)
#PACF-> AR(0)
#I-> 1 based on diff level
#In my case 0,1,1
#maximum likelihood(ML) // minimize conditional sum-of-squares(CSS)
fitArima<-arima(NetSatis.timeseries, order = c(0,1,1), seasonal = list(order = c(1,0,0), period=12), method="ML" )
fitArima

#significance of coefficients
coeftest(fitArima)
#confint(fitArima)
par(mfrow = c(1,1))
acf(fitArima$residuals)

#NOTE: If residuals(errors) are above 0.05 that means we are in the right way
#residual diagnostic
boxresult<- LjungBoxTest(fitArima$residuals, k= 2,StartLag =1)
par(mfrow= c(2,1))
plot(boxresult[,3], main= "Ljung Box Q Test", ylab="P-values", xlab="Lag")
qqnorm(fitArima$residuals)
qqline(fitArima$residuals)

auto.arima(NetSatis.timeseries, trace= TRUE )

#forecast future values
par(mfrow = c(1,1))
predict(fitArima, n.ahead = 12)
futureVal<- forecast(fitArima, h=12, level = c(99.5))
plot(futureVal)
futureVal












