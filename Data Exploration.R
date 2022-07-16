library(tidyverse)
library(tsibble)
library(fable)
library(ggplot2)
library(lubridate)
library(fpp3)
library(seasonal)

Monthly <- read_csv("Monthly.csv")
Monthly$Date <- 
  parse_date(Monthly$Date, format = "%d/%m/%y")

Monthly <- Monthly %>% 
  mutate(
    Date = yearmonth(Date),
    Throughput = Throughput/1000000
  )
Monthly_ts <- as_tsibble(Monthly, index = "Date")

autoplot(Monthly_ts, Throughput) + 
  labs(title = "US Airline Passenger Throughput",
       subtitle = "January 2019 - May 2022",
       y = "Monthly passengers (Millions)")

Monthly_ts %>% 
  ACF(Throughput, lag_max = 120) %>% 
  autoplot()

# The data is so limited and influenced by the COVID episode that it's very hard to take these models without the greatest grain of salt on the planet. 
# They are fashioned more to provide a springboard for further thinking about the data.

ETS_model <- Monthly_ts %>% 
  model(ETS(Throughput))
components(ETS_model) %>% 
  autoplot() +
  labs(title = "ETS Components Throughput")
ETS_model %>% 
  forecast(h = 6) %>% 
  autoplot(Monthly_ts) +
  labs(title = "2022 Passenger Throughput Forecast")

ARIMA <- Monthly_ts %>% 
  model(ARIMA(Throughput, stepwise=FALSE, greedy=FALSE, approximation=FALSE)) %>% 
  report(ARIMA)

ARIMA %>% 
  forecast(h=6) %>% 
  autoplot(Monthly_ts) +
  labs(title = "Forecast of Dec 2022 US Airline Passengers", subtitle = "ARIMA (2,0,0) with mean") + 
  coord_cartesian(xlim = as.numeric(as.Date(c("2019-01-01", "2022-12-01"))), ylim = c(30, 80))

ARIMA %>% 
  gg_tsresiduals()



