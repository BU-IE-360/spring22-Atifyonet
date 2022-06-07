require(data.table)
require(forecast)
require(ggplot2)
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
baseline_forecast=production_with_forecast[date==latest_available_prod_date]$production
forecast_table[,baseline:=baseline_forecast]
forecast_table

ggplot(production_with_forecast ,aes(x=date)) +
  geom_line(aes(y=production,color='real')) + 
  geom_line(aes(y=production,color='predicted'))
