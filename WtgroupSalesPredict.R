library(tseries)
library(urca)



path_in_filename = "wtGroup.csv"
VuSales<-read.csv(path_in_filename)

#creating timeseries
rownames(VuSales)<-VuSales$AyYil
NetSatis.timeseries<- ts(VuSales[,3], frequency = 12, start= c(2019,1))
autoplot(NetSatis.timeseries)

####
#Stationary testing
####

#Stationary testing-ADF
#if p-value smaller than 0.05 then its stationary
adf = adf.test(NetSatis.timeseries)
print(adf)
#stationary testing on first difference
NetSatis.diff_stationary<-diff(NetSatis.timeseries)
NetSatis.diff_stationary<- na.omit(NetSatis.diff_stationary)
diff.adf<-adf.test(NetSatis.diff_stationary)
print(diff.adf)
#run this part until p-value < 0.05 (stationary)
NetSatis.diff_stationary<-diff(NetSatis.diff_stationary)
NetSatis.diff_stationary<-na.omit(NetSatis.diff_stationary)
diff.adf<- adf.test(NetSatis.diff_stationary)
print(diff.adf)
#I(4)-> integration level meaning we have difference level of 4

#plot data
par(mfrow = c(2,1), mar= c(3,4,4,2))
plot(NetSatis.timeseries, col="darkblue", ylab="Amount")
plot(NetSatis.diff_stationary, col="darkblue", ylab= "Amount")

#Stationary testing-ur.df
#To be stationary value of "test-statistic value" must be lower than "1pc/tau1" 1percent
urdf<-ur.df(NetSatis.timeseries, type="none", selectlags = "AIC")
summary(urdf)
#make differences on series // lag and differences 1 is default value
urdf.diff_stationary<-diff(NetSatis.timeseries, lag= 1, differences = 1)
#plot the graph
plot(urdf.diff_stationary, col="darkgreen", xlab="Time Period", ylab="urdf.diff_stationary", type="l", lwd=1,
     cex= 1, main="NetSatis in on First Differences from 2019-2020", cex.axis = .5)

urdf <- ur.df(urdf.diff_stationary, type="none", selectlags = "AIC")
summary(urdf)
#I(1)-> integration level meaning we have difference level of 1


####
#Stationary testing-end
####

###
#Picking best Arima-model
###

##UR.DF

#finding AR,MA for ur.df
ggAcf(urdf.diff_stationary, lag.max = 12)+ theme_bw()
ggPacf(urdf.diff_stationary, lag.max = 12)+theme_bw()
#MA->ACF results(1,4,5) lags
#AR-> PACF results(1,4) lags

#Testing Arima models for ur.df
#ARIMA(AR/I/MA)->AutoRegressive (p)- Integrated(q)- Moving Average(d)
#Prequisite: Data should be stationary
#Fit ARIMA Model
ARIMA1<- Arima(NetSatis.timeseries, order = c(1,1,1))
ARIMA2<- Arima(NetSatis.timeseries, order = c(1,1,4))
ARIMA3<- Arima(NetSatis.timeseries, order = c(1,1,5))
ARIMA4<- Arima(NetSatis.timeseries, order = c(4,1,1))
ARIMA5<- Arima(NetSatis.timeseries, order = c(4,1,4))
ARIMA6<- Arima(NetSatis.timeseries, order = c(4,1,5))
#Display ARIMA result
summary(ARIMA1)
summary(ARIMA2)
summary(ARIMA3)
summary(ARIMA4)
summary(ARIMA5)
summary(ARIMA6)
#based on RMSE: ARIMA5
#based on AIC & BIC: ARIMA1
#based on AICc:ARIMA1

##ADF

#finding AR,MA for adf
ggAcf(NetSatis.diff_stationary, lag.max = 12)+ theme_bw()#1,4,5
ggPacf(NetSatis.diff_stationary, lag.max = 12)+theme_bw()#1,2
#I(4)
#Testing Arima models for adf
A1<- Arima(NetSatis.timeseries, order = c(1,4,1))
A2<- Arima(NetSatis.timeseries, order = c(1,4,4))
A3<- Arima(NetSatis.timeseries, order = c(1,4,5))
A4<- Arima(NetSatis.timeseries, order = c(2,4,1))
A5<- Arima(NetSatis.timeseries, order = c(2,4,4))
A6<- Arima(NetSatis.timeseries, order = c(2,4,5))
#Display ARIMA result
summary(A1)
summary(A2)
summary(A3)
summary(A4)
summary(A5)
summary(A6)
#based on RMSE: A6
#based on AIC & BIC: A6
#based on AICc:A4



##Auto Arima

#building forecast graph
#find best-fitted arima model
Fitted.Arima = auto.arima(NetSatis.timeseries)
#Plot forecasted ARIMA with the last 12 months
auto.forecast = forecast(Fitted.Arima, h=12)
summary(auto.forecast)
plot(auto.forecast, include = 24)


####
#Forecasting by using best Arima models
####

#ur.df-ARIMA model
#based on RMSE: ARIMA5
urdf.RMSE_forecast<-forecast(ARIMA5, h= 12)
summary(urdf.RMSE_forecast)
plot(urdf.RMSE_forecast, include =24)
#based on AIC & BIC: ARIMA1
#based on AICc:ARIMA1
urdf.AICandAICc_forecast<-forecast(ARIMA1, h= 12)
summary(urdf.AICandAICc_forecast)
plot(urdf.AICandAICc_forecast, include =24)

#adf-ARIMA model
#based on RMSE: A6
#based on AIC & BIC: A6
adf.RMSEandAIC_forecast<-forecast(A6, h= 12)
summary(adf.RMSEandAIC_forecast)
plot(adf.RMSEandAIC_forecast, include =24)
#based on AICc:A4
adf.AICc_forecast<-forecast(A4, h= 12)
summary(adf.AICc_forecast)
plot(adf.AICc_forecast, include =24)


