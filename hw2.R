install.packages("corrplot")
install.packages("ggcorrplot")
install.packages("GGally")
install.packages("forecast")
install.packages("dplyr")
install.packages("RcppRoll")
install.packages("readxl")
install.packages("lubridate")
install.packages("zoo")
install.packages("ggplot2")
install.packages("scales")
install.packages("data.table")
library(corrplot)
library(ggcorrplot)
library(GGally)
library(forecast)
library(dplyr)
library(RcppRoll)
library(readxl)
library(lubridate)
library(zoo)
library(ggplot2)
library(scales)
library(data.table)
getwd()
data <- read.csv("IE360_Spring22_HW2_data.csv",colClasses=c('character',rep('numeric',10)))
summary(data)
data
data$Quarter=as.Date(as.yearqtr(data$Quarter,format="%Y_Q%q"))
datatr=data.table(data[c(1:28),])
datafore=data.table(data[c(29,30,31,32),])
str(datatr)
str(datafore)
ggp1 <- ggplot(data = datatr,aes(x = Quarter,y = Unleaded.Gasoline.Sale..UGS.))
ggp1 + geom_line(color ="yellow") +
  geom_smooth(fill = NA, color="blue",linetype = "twodash", size = 0.5) +
  labs(title = "UGS & Quarter",
       x = "Quarter",
       y = "UGS" )
mseries=roll_mean(datatr$Unleaded.Gasoline.Sale..UGS.,4,align='left')
plot(mseries,
     type='l',col='black',
     xlab = "time",
     ylab = "UGS rolling mean")
varseries=roll_var(datatr$Unleaded.Gasoline.Sale..UGS.,4,align='left')
plot(varseries,
     type='l',col='black',
     xlab = "time",
     ylab = "UGS rolling variance"
)
acf(datatr$Unleaded.Gasoline.Sale..UGS.)
datatr[,trend := 1:.N ]
datatr[,Quarter_:=as.character(month(Quarter))]
datafore[,trend := 29:32 ]
datafore[,Quarter_:=as.character(month(Quarter))]
model1 <- lm(Unleaded.Gasoline.Sale..UGS. ~ trend+Quarter_, datatr)
model1
summary(model1)
PM1=copy(datatr)
PM1[,actual:=Unleaded.Gasoline.Sale..UGS.]
PM1[,predicted_trend:=predict(model1,PM1)]
PM1[,residual_trend:=actual-predicted_trend]
ggplot(PM1 ,aes(x=Quarter)) +
  geom_line(aes(y=Unleaded.Gasoline.Sale..UGS.,color='real')) + 
  geom_line(aes(y=predicted_trend,color='predicted'))
checkresiduals(model1$residuals)
ggpairs(datatr)
model2 <- lm(Unleaded.Gasoline.Sale..UGS. ~ trend+ X..LPG.Vehicles..NLPG.+Price.of.Unleaded.Gasoline..PU.+Price.of.Diesel.Gasoline..PG.+X..Unleaded.Gasoline.Vehicles..NUGV.+X..of.Diesel.Gasoline.Vehicles..NDGV.+GNP.Agriculture + Quarter_, datatr)
summary(model2)

PM2=copy(datatr)
PM2[,actual:=Unleaded.Gasoline.Sale..UGS.]
PM2[,predicted_trend:=predict(model2,PM2)]
PM2[,residual_trend:=actual-predicted_trend]

ggplot(PM2 ,aes(x=Quarter)) +
  geom_line(aes(y=Unleaded.Gasoline.Sale..UGS.,color='real')) + 
  geom_line(aes(y=predicted_trend,color='predicted'))

checkresiduals(model2$residuals)

datatr$Unleaded.Gasoline.Sale..UGS.lag1=lag(datatr$Unleaded.Gasoline.Sale..UGS.,1)

datatr$X..Unleaded.Gasoline.Vehicles..NUGV.lag5=lag(datatr$X..Unleaded.Gasoline.Vehicles..NUGV.,5)
datatr$X..of.Diesel.Gasoline.Vehicles..NDGV.lag5=lag(datatr$X..of.Diesel.Gasoline.Vehicles..NDGV.,5)

model3 <-lm(Unleaded.Gasoline.Sale..UGS. ~ trend+Price.of.Unleaded.Gasoline..PU.+Price.of.Diesel.Gasoline..PG.+X..Unleaded.Gasoline.Vehicles..NUGV.+X..of.Diesel.Gasoline.Vehicles..NDGV.+GNP.Agriculture +Unleaded.Gasoline.Sale..UGS.lag1+X..Unleaded.Gasoline.Vehicles..NUGV.lag5+X..of.Diesel.Gasoline.Vehicles..NDGV.lag5 + Quarter_, datatr)
summary(model3)
checkresiduals(model3$residuals)

PM3=copy(datatr)
PM3[,actual:=Unleaded.Gasoline.Sale..UGS.]
PM3[,predicted_trend:=predict(model3,PM3)]
PM3[,residual_trend:=actual-predicted_trend]

ggplot(PM3 ,aes(x=Quarter)) +
  geom_line(aes(y=Unleaded.Gasoline.Sale..UGS.,color='real')) + 
  geom_line(aes(y=predicted_trend,color='predicted'))

datafore$Unleaded.Gasoline.Sale..UGS.lag1[1]=datatr$Unleaded.Gasoline.Sale..UGS.[28]

datafore$X..Unleaded.Gasoline.Vehicles..NUGV.lag5[1]=datatr$X..Unleaded.Gasoline.Vehicles..NUGV.[24]
datafore$X..of.Diesel.Gasoline.Vehicles..NDGV.lag5[1]=datatr$X..of.Diesel.Gasoline.Vehicles..NDGV.[24]



datafore[1,"Unleaded.Gasoline.Sale..UGS."]=as.numeric(predict(model3,newdata=datafore[1,]))
datafore$Unleaded.Gasoline.Sale..UGS.lag1[2]=as.numeric(datafore[1,"Unleaded.Gasoline.Sale..UGS."])


datafore[2,"Unleaded.Gasoline.Sale..UGS."]=predict(model3,newdata=datafore[2,])
datafore$Unleaded.Gasoline.Sale..UGS.lag1[3]=as.numeric(datafore[2,"Unleaded.Gasoline.Sale..UGS."])

datafore[3,"Unleaded.Gasoline.Sale..UGS."]=predict(model3,newdata=datafore[3,])
datafore$Unleaded.Gasoline.Sale..UGS.lag1[4]=as.numeric(datafore[3,"Unleaded.Gasoline.Sale..UGS."])

datafore[4,"Unleaded.Gasoline.Sale..UGS."]=predict(model3,newdata=datafore[4,])
datafore[,"Unleaded.Gasoline.Sale..UGS."]

