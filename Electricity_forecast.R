setwd(dir= "C:/Users/khanh/OneDrive/Desktop/IB BOOKS/Sales & Operations Planning/Forecast_assigment")

library(tsibble)
library(lubridate)
library(dplyr)
library(forecast)
library(ggplot2)
library("fpp3")
library(base)

consumption <- read.csv("consumption.csv")

forecasts <- read.csv("forecasts.csv")

forecasts$date <- as.Date(forecasts$Forecast.day.ahead, format = "%d/%m/%Y")


daily_temperature_helsinki <- aggregate(Helsinki ~ date, data = forecasts, FUN = mean)
daily_temperature_rovaniemi <- aggregate(Rovaniemi ~ date, data = forecasts, FUN = mean)
daily_temperature_tampere <- aggregate(Tampere ~ date, data = forecasts, FUN = mean)
daily_forecasts <- merge(merge(daily_temperature_helsinki, daily_temperature_tampere, by = "date"), daily_temperature_rovaniemi, by = "date")
daily_forecasts <- daily_forecasts |>
  mutate(wday(date))

# Add a new column indicating weekends as 1 and weekdays as 0
daily_forecasts <- daily_forecasts %>%
  mutate(
    Weekend = ifelse(wday(date) %in% c("1", "7"), 1, 0))
daily_forecasts_ts <- daily_forecasts |> as_tsibble(index = date)


# Convert to datetime objects
consumption$Start.time.UTC <- ymd_hms(consumption$Start.time.UTC)
consumption$End.time.UTC <- ymd_hms(consumption$End.time.UTC)

consumption$date <- as.Date(consumption$Start.time.UTC)
consume <- consumption |> 
  group_by(date) |>
  summarize(total_value = mean(Electricity.consumption.in.Finland)*24)

# Assign day of the week using weekdays() function
consume <- consume |>
  mutate(wday(date))

view(consume)

# Add a new column indicating weekends as 1 and weekdays as 0
consume <- consume %>%
  mutate(
    Weekend = ifelse(wday(date) %in% c("1", "7"), 1, 0))

view(consume)

consume$log_value <- log(consume$total_value)

consume_ts <- consume |> as_tsibble(index = date)

autoplot(consume_ts, `log_value`)

#Train Exponential Smoothing model

etsANA <- head(consume_ts, -1) |>
  model(ETS(log_value ~ error("A") + trend("N") + season("A")))

fc <- etsANA |>
  forecast(h = "1 years")

fc |>autoplot(consume_ts)

etsANA_1 <- consume_ts |>
  model(ETS(log(total_value) ~ error("A") + trend("N") + season("A")))

accuracy(etsANA_1)
fc_1 <- etsANA_1 |>
  forecast(h = "2 days")

fc_1 |> accuracy(consume_ts) 
fc_1 |>autoplot(consume_ts)


#Importing and cleaning weather data

helsinki_weather <- read.csv("Helsinki Kaisaniemi observations.csv")
rovaniemi_weather <- read.csv("Rovaniemi rautatieasema observations.csv")
tampere_weather <- read.csv("Tampere Siilinkari observations.csv")


helsinki_weather$date <- as.Date(paste(helsinki_weather$Year, helsinki_weather$Month, helsinki_weather$Day, sep = "-"))
rovaniemi_weather$date <- as.Date(paste(rovaniemi_weather$Year, rovaniemi_weather$Month, rovaniemi_weather$Day, sep = "-"))
tampere_weather$date <- as.Date(paste(tampere_weather$Year, tampere_weather$Month, tampere_weather$Day, sep = "-"))

daily_helsinki <- aggregate(Mean.temperature ~ date, data =helsinki_weather , FUN = mean)
colnames(daily_helsinki)[2] <- "helsinki_temperature"

daily_rovaniemi <- aggregate(Mean.temperature ~ date, data = rovaniemi_weather, FUN = mean)
colnames(daily_rovaniemi)[2] <- "rovaniemi_temperature"

daily_tampere <- aggregate(Mean.temperature ~ date, data = tampere_weather, FUN = mean)
colnames(daily_tampere)[2] <- "tampere_temperature"

daily_weather <- merge(merge(daily_helsinki, daily_tampere, by = "date"), daily_rovaniemi, by = "date")

# Assign day of the week using weekdays() function
daily_weather <- daily_weather |>
  mutate(wday(date))

view(daily_weather)

# Add a new column indicating weekends as 1 and weekdays as 0
daily_weather <- daily_weather %>%
  mutate(
    Weekend = ifelse(wday(date) %in% c("1", "7"), 1, 0))

daily_weather_ts <- daily_weather |> as_tsibble(index = date)

consumption_weather <- merge(consume_ts, daily_weather_ts, by = "date")

consumption_weather_ts <- consumption_weather|> as_tsibble(index = date)

date_diff <- diff(consumption_weather$date)

# Check if any difference is not exactly by 1
if (any(date_diff != 1)) {
  cat("Date column has differences not only by 1 day.")
} else {
  cat("Date column has differences only by 1 day.")
}

consumption_weather_ts <- consumption_weather_ts %>%
 group_by_key() %>%
 fill_gaps() %>%
 tidyr::fill(total_value, helsinki_temperature, tampere_temperature, rovaniemi_temperature, .direction = "down")

#Train linear regression model

fit <- consumption_weather_ts |>
  model(TSLM(total_value ~ helsinki_temperature + tampere_temperature + rovaniemi_temperature + Weekend))
fit |> report()

fc_tslm <- forecast(fit, new_data = )

    
ts_data <- ts(consumption_weather_ts$total_value, frequency = 1)  # data is daily, set frequency to 1

# Create a data frame with your exogenous regressors
exog_matrix <- cbind(
  consumption_weather_ts$helsinki_temperature,
  consumption_weather_ts$tampere_temperature,
  consumption_weather_ts$rovaniemi_temperature,
  consumption_weather_ts$Weekend.x
)

# Create a data frame with forecast exogenous regressors
exog_matrix_forecast <- cbind(
  tail(daily_forecasts_ts$Helsinki, 2),
  tail(daily_forecasts_ts$Tampere, 2),
  tail(daily_forecasts_ts$Rovaniemi, 2),
  tail(daily_forecasts_ts$Weekend, 2)
)

# Fit ARIMAX model with the modified data
model <- auto.arima(log(ts_data), xreg = exog_matrix)

# Print model summary
summary(model)

forecast_values <- forecast(model, xreg = exog_matrix_forecast)

autoplot(log(ts_data)) +
  autolayer(forecast_values, series = "Forecast") +
  xlab("Date") +
  ylab("Total Value") +
  ggtitle("Total Value with Forecasts")

# Extract the forecasted value
forecast_value <-forecast_values$mean[2]
print(forecast_value)

# Extract the lower and upper prediction intervals
lower_pi <- forecast_values$lower[2]
upper_pi <- forecast_values$upper[2]

# Calculate the standard deviation of the forecast
forecast_sd <- (upper_pi - lower_pi) / (2 * qnorm(0.5 + 0.95 / 2))

# Print the forecast standard deviation
print(forecast_sd)

