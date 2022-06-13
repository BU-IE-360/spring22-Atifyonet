install.packages("RcppRoll")
install.packages("tidyr")
install.packages("readxl")
install.packages("lubridate")
install.packages("zoo")
install.packages("ggplot2")
install.packages("scales")
install.packages("data.table")
install.packages("corrplot")
install.packages("ggcorrplot")
install.packages("GGally")
install.packages("forecast")
install.packages("dplyr")
library(RcppRoll)
library(tidyr)
library(readxl)
library(lubridate)
library(zoo)
library(ggplot2)
library(scales)
library(data.table)
library(corrplot)
library(ggcorrplot)
library(GGally)
library(forecast)
library(dplyr)


production=fread("2022-06-13_production.csv")
weather=fread("2022-06-13_weather.csv")





production = production[order(date,hour)]
production = production[,month:= as.factor(month(date))]
production = production[,quart:= as.factor(quarter(date))]
head(production,3)

wide_weather=dcast(weather,date+hour~variable+lat+lon,value.var='value')

production_with_weather=merge(production,wide_weather,by=c('date','hour'))
train_data=production_with_weather[date<'2022-03-01']
test_data=production_with_weather[date>='2022-03-01' & date<='2022-05-24']
tail(train_data[,1:6],3)
head(test_data[,1:6],3)
tail(test_data[,1:6],2)
nrow(train_data) 
nrow(test_data)

arima_model = auto.arima(train_data$production,seasonal=F,trace=T,stepwise=T,approximation=T)
arima_model
checkresiduals(arima_model)
install.packages("urca")
library(urca)
train_data[,diff_series:=production-shift(production,24)]

unt_test1=ur.kpss(train_data$diff_series) 
summary(unt_test1)
sarima_model=auto.arima(train_data$diff_series,seasonal=T,trace=T,stepwise=T,approximation=T)
sarima_model
checkresiduals(sarima_model)

train_data[,dwsrf_diff:=`DSWRF_36.5_33.25`-shift(`DSWRF_36.5_33.25`,24)]
tail(train_data)

reg_matrix=cbind(train_data$dwsrf_diff)

sarimax_model= auto.arima(train_data$diff_series,xreg=reg_matrix,seasonal=T,trace=T,stepwise=T,approximation=T)
sarimax_model
checkresiduals(sarimax_model)