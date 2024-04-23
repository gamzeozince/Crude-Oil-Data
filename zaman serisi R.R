

getwd()
setwd("C:/Users/gamze/OneDrive/Masaüstü")
list.files()
oil<-read.csv("oil.csv"   ,header=TRUE)
oilseries<- ts(oil) 
plot.ts(oilseries)
oilcomponents<- decompose(oilseries)
plot(oilcomponents)
oilcomponents$seasonal

oilseriesdiff1<-diff(oilseries,differences =1)
plot.ts(oilseriesdiff1)
oilseriesdiff1<-diff(oilseries,differences =2)
plot.ts(oilseriesdiff2)
acf(oilseriesdiff1,lag.max = 50)
pacf(oilseriesdiff1,lag.max = 50)
acf(oilseriesdiff2,lag.max = 50)
pacf(oilseriesdiff2,lag.max = 50)
adf.test(oilseriesdiff1)
adf.test(oilseriesdiff2)
sim.ar<-arima.sim(list(ar=c(0.5)),n=100)
acf(sim.ar,main="ACF of AR(1) process")
pacf(sim.ar,main="ACF of AR(1) process")
sim.ar<-arima.sim(list(ar=c(0.4,0.4)),n=1000)
acf(sim.ar,main="ACF of AR(2) process")
pacf(sim.ar,main="ACF of AR(2) process")

library(IMTest)
arima_model1<- arima(oilseries,order = c(1,1,0))
arima_model1
arima_model2<- arima(oilseries,order = c(0,1,1))
arima_model2
arima_model3<- arima(oilseries,order = c(1,1,1))
arima_model3
arima_model4<- arima(oilseries,order = c(0,2,1))
arima_model4
tsdiag(arima_model3)
tsdiag(arima_model2)


tsdiag(arima_model4)
Box.test(arima_model3$residuals, lag=6)
Box.test(arima_model3$residuals, lag=12)
Box.test(arima_model3$residuals, lag=24)
shapiro.test(arima_model3$residuals)
qqnorm(arima_model3$residuals)
Box.test(arima_model2$residuals, lag=6)
Box.test(arima_model2$residuals, lag=12)
checkresiduals(arima_model3)
forecast(arima_model3,h=5)
plot(forecast(arima_model3,h=5))
predict(arima_model3, n.ahead=5)
accuracy(forecast(arima_model3,h=5))
