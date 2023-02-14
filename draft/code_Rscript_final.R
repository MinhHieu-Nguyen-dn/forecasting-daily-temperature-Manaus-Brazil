#Install libraries
install.packages('fpp2')
library(fpp2)

install.packages('tidyverse')
library(tidyverse)

install.packages('GGally')
library(GGally)

install.packages('ggplot2')
library(ggplot2)

install.packages('xts')
library(xts)

#Read pre-processed data and assign time-series variables
data = read.csv(file='data_pre-processed/data_daily_avg_from2000_365_withdate.csv')
temp_ts = ts(data = data$Avg_temp, start = c(2000,1), frequency = 365)
#dates = as.Date(data$Date)
#data = xts(data[, -1], dates, frequency = 365)

#Separate data into train/test sets
#long-term forecasting
data_train1 = data[row.names(data) %in% 1:6935, ]
data_test1 = data[row.names(data) %in% 6936:nrow(data), ]

#short-term forecasting
data_train2 = data[row.names(data) %in% 1:7295, ]
data_test2 = data[row.names(data) %in% 7296:nrow(data), ]


#Visualize time-series objects on plots
data$Avg_temp %>% autoplot() + ggtitle('Daily Average Temperature (Celsius degree) at Manaus station') + xlab('Date') + ylab('Temperature')
data$Humidity %>% autoplot() + ggtitle('Daily Average Humidity at Manaus station') + xlab('Date') + ylab('Humidity')
data$Atmospheric_pressure %>% autoplot() + ggtitle('Daily Average Atm Pressure at Manaus station') + xlab('Date') + ylab('Atm Pressure')
data$Wind_speed %>% autoplot() + ggtitle('Daily Average Wind Speed at Manaus station') + xlab('Date') + ylab('Speed')
data$Cloudiness %>% autoplot() + ggtitle('Daily Average Cloudiness at Manaus station') + xlab('Date') + ylab('Cloudiness')

#Bench marking methods - long-term forecasting


#Average method
model_mean1 = meanf(temp_ts, h=365)
#Naive method
model_naive1 = naive(temp_ts, h=365)
#Seasonal naive
model_snaive1 = snaive(temp_ts, h=365)
#Drift
model_rwf1 = rwf(temp_ts, h=365, drift=T)

autoplot(temp_ts) +
  autolayer(model_mean1, series='Mean', PI=F) +
  autolayer(model_naive1, series='Naive', PI=F) +
  ggtitle('Forecasts from bench marking methods') +
  ylab('Temperature (Celsicus degree)') +
  guides(colour=guide_legend(title='Bench marking methods'))


autoplot(temp_ts) +
  autolayer(model_snaive1, series='Seasonal Naive', PI=F) +
  ggtitle('Forecasts from bench marking methods') +
  ylab('Temperature (Celsicus degree)') +
  guides(colour=guide_legend(title='Bench marking methods'))


autoplot(temp_ts) +
  autolayer(model_rwf1, series='Drift', PI=F) +
  ggtitle('Forecasts from bench marking methods') +
  ylab('Temperature (Celsicus degree)') +
  guides(colour=guide_legend(title='Bench marking methods'))


#Timeseries regression
data_train_lm = window(data, start=c(2000,1), end=c(2018,365))
data_test_lm = window(data, start=c(2019,1))

model_lm <- lm(Avg_temp ~ Humidity + Atmospheric_pressure + Wind_speed + Cloudiness, data = data)
h <- 365

f.lm <- forecast(model_lm,
                      newdata = data.frame(
                        Humidity = rep(mean(data[,"Humidity"]), h),
                        Atmospheric_pressure = rep(mean(data[,"Atmospheric_pressure"]), h),
                        Wind_speed = rep(mean(data[,"Wind_speed"]), h),
                        Cloudiness = rep(mean(data[,"Cloudiness"]), h)
                      ))
autoplot(data[,"Avg_temp"]) +
  ylab("% change temp") +
  autolayer(fcast.ave, series = "Average increase",
            PI = TRUE) +
  guides(colour = guide_legend(title = "Scenario"))




