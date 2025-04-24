# Install required packages (run only once)
install.packages(c("ggplot2", "forecast", "fable", "tsibble", "fpp3"))

# Load the packages
library(ggplot2)
library(forecast)
library(tsibble)
library(fable)
library(lubridate)
library(dplyr)
library(fpp3)  # this loads tsibble, fable, feasts, and distributional

# Use a built-in dataset (e.g., Australian beer production)
data <- aus_production %>% filter(!is.na(Beer))

# Step 1: Visualize time series
data %>% 
  autoplot(Beer) +
  ggtitle("Beer Production Over Time") +
  xlab("Year") + ylab("Megalitres")

# Step 2: Convert to time series object (tsibble is already used)
# already in tsibble format, no conversion needed

# Step 3: Check stationarity (ADF test)
beer_ts <- ts(data$Beer, frequency = 4)  # Quarterly data
adf.test(beer_ts)  # from tseries package, install it if needed

# Step 4a: Fit ARIMA model
fit_arima <- data %>%
  model(ARIMA(Beer))

# Step 4b: Fit ETS model (Exponential Smoothing)
fit_ets <- data %>%
  model(ETS(Beer))

# Step 5: Forecast 8 periods (2 years if quarterly)
fc_arima <- forecast(fit_arima, h = 8)
fc_ets <- forecast(fit_ets, h = 8)

# Plot forecasts
fc_arima %>%
  autoplot(data) +
  ggtitle("ARIMA Forecast")

fc_ets %>%
  autoplot(data) +
  ggtitle("ETS Forecast")

# Step 6: Model accuracy (training set only)
accuracy(fit_arima)
accuracy(fit_ets)
