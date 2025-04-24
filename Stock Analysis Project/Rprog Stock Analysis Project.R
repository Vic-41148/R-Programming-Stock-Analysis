# Install packages if not already installed
# install.packages("quantmod")
# install.packages("TTR")

library(quantmod)
library(TTR)

# Step 1: Fetch Stock Data (for Apple Inc.)
getSymbols("AAPL", src = "yahoo", from = "2022-01-01", to = Sys.Date())

# Step 2: Calculate Moving Averages
AAPL$SMA50 <- SMA(Cl(AAPL), n = 50)   # 50-day simple moving average
AAPL$SMA200 <- SMA(Cl(AAPL), n = 200) # 200-day simple moving average

# Step 3: Visualize Stock Prices with Moving Averages
chartSeries(AAPL, TA = NULL, theme = chartTheme("white"))
addSMA(n = 50, col = "blue")
addSMA(n = 200, col = "red")

