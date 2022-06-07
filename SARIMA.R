require(data.table)
require(forecast)
todays_date=Sys.Date()
forecast_date=todays_date+1
production_file=read.csv("productionnew.csv")
weather_file=read.csv("weathernew.csv")
production=fread("productionnew.csv")
weather=fread("weathernew.csv")
latest_available_prod_date=as.Date(max(production$date))
n_days=as.numeric(forecast_date-latest_available_prod_date)
forecasted_production=tail(production,n_days*24)
forecasted_production[,date:=date+n_days]
forecasted_production[,production:=NA]
production_with_forecast=rbind(production,forecasted_production)
forecast_table=data.table(date=forecast_date,hour=0:23,production=NA)
production_with_forecast=production_with_forecast[order(date,hour)]
production_series=ts(production_with_forecast[!is.na(production)]$production,frequency=24)
sarima_model=auto.arima(production_series,seasonal=T,stepwise=T,approximation=T,trace=T)
forecast_ahead=nrow(forecast_table)
sarima_forecast=forecast(sarima_model,h=forecast_ahead)
forecast_table[,sarima:=tail(sarima_forecast$mean,24)]
forecast_table


ggplot(production_with_forecast ,aes(x=date)) +
  geom_line(aes(y=production,color='real')) + 
  geom_line(aes(y=production,color='predicted'))


0
0
0
0
0
0
6.7
26.8


9: 2022-05-27    8         NA 35.04514217
10: 2022-05-27    9         NA 35.04644723
11: 2022-05-27   10         NA 35.05464172
12: 2022-05-27   11         NA 34.41357885
13: 2022-05-27   12         NA 34.15072706
14: 2022-05-27   13         NA 28.23162718
15: 2022-05-27   14         NA 29.63710050
16: 2022-05-27   15         NA 27.43127603
17: 2022-05-27   16         NA 28.24020473
18: 2022-05-27   17         NA 12.24364147
19: 2022-05-27   18         NA  6.12929802
20: 2022-05-27   19         NA  1.05257247
21: 2022-05-27   20         NA  0.05782660
22: 2022-05-27   21         NA  0.05788655
23: 2022-05-27   22         NA  0.05793293
24: 2022-05-27   23         NA  0.05796881

