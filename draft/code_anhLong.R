data <- read.csv("data_pre-processed/data_daily_avg_from2000_365_withdate.csv")
x_ts <- ts(data = data$Avg_temp, start = c(2000,1,1), frequency = 365)


model1 = arima(x_ts,c(5,1,1))
autoplot(forecast(model1))


model2 = ets(x_ts,"MAN")
autoplot(forecast(model2))

x_ts <- ts(data = data, start = c(2000,1,1), frequency = 365)

fit.temp <- tslm(Avg_temp ~ Humidity + Atmospheric_pressure + Wind_speed + Cloudiness, data = x_ts)
h <- 365
fcast.ave <- forecast(fit.temp,
                      newdata = data.frame(
                        Humidity = rep(mean(x_ts[,"Humidity"]), h),
                        Atmospheric_pressure = rep(mean(x_ts[,"Atmospheric_pressure"]), h),
                        Wind_speed = rep(mean(x_ts[,"Wind_speed"]), h),
                        Cloudiness = rep(mean(x_ts[,"Cloudiness"]), h)
                      ))
autoplot(x_ts[,"Avg_temp"]) +
  ylab("% change temp") +
  autolayer(fcast.ave, series = "Average increase",
            PI = TRUE) +
  guides(colour = guide_legend(title = "Scenario"))


x_ts <- ts(data = data$Avg_temp, start = c(2000,1,1), frequency = 365)
x_ts %>%
  stl(t.window=365, s.window="periodic", robust=TRUE) %>%
  autoplot()
fit <- stl(x_ts, t.window=365, s.window="periodic",
           robust=TRUE)
fit %>% seasadj() %>% naive() %>%
  autoplot() + ylab("temp") +
  ggtitle("temp")
fit %>% forecast(method="naive") %>%
  autoplot() + ylab("New orders index")