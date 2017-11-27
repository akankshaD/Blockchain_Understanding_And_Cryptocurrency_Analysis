# Loading libraries
library(readr)
library(MASS)
library(quantmod)
library(knitr)

# Cleaning the environment by removing existing variables
rm(list=ls())

# Setting the directory
setwd('/home/akanksha/Desktop/Sem III/Data Analytics/Course Project/Dataset/cryptocurrencypricehistory')

# Load dataset
eth <- read_csv('ethereum_price.csv')
eth$Date <- format(as.Date(eth$Date, format= "%B %d,%Y"), format = "%Y-%m-%d")    # changing the format of date
head(eth)

# Visualize Close attribute
N <- nrow(eth)
Eth_Close <- eth$Close[N:1]
plot(Eth_Close, type = 'l')

# Stabilizing the variance
plot(diff(Eth_Close, differences = 4), type = 'l', main = 'Original data')


plot(diff(log(Eth_Close)), type = 'l', main = 'Log-transformed data') 

plot(diff(sqrt(Eth_Close)), type= 'l', main = 'Square root transformed data') 

# Computing Auto Corelation Factor for constant variance data
eth_close_diff <- diff(sqrt(Eth_Close))

acf(eth_close_diff)

# To check seasonality of data
# You can compute the average difference between the timestamps, and check if it is closer to 1 (daily data), 7 (weekly), etc.

guess_period <- function(x) { 
  average_period <- as.double( mean(diff(index(x))), units="days" )
  difference <- abs(log( average_period / c(
    daily = 1,
    business_days = 7/5,
    weekly = 7,
    monthly = 30,
    quarterly = 365/4,
    annual = 365
  ) ) )
  names( which.min( difference ) )
}

# Eliminate noise and isolate true signal (conversion into time series)
eth_close_ts <- ts(eth_close_diff,frequency = 365,  start = 2015-08-07)

# Decompose a time series into seasonal, trend and irregular components
eth_close_de <- decompose(eth_close_ts)

plot(eth_close_de)

# Use AIC to choose P and Q and model ARIMA
# AIC provides a means for model selection

eth_close_rand <- eth_close_de$random

aic_table <- function(data,P,Q){
  table <- matrix(NA,(P+1),(Q+1))
  for(p in 0:P) {
    for(q in 0:Q) {
      table[p+1,q+1] <- arima(data,order=c(p,0,q))$aic
    }
  }
  dimnames(table) <- list(paste("<b> AR",0:P, "</b>", sep=""),paste("MA",0:Q,sep=""))
  table
}

eth_aic_table <- aic_table(eth_close_rand, 6, 6)

kable(eth_aic_table, digits = 2)

# Selecting the model with lowest AIC value ARMA(2,2)
arma22 <- arima(eth_close_rand, order = c(2, 0, 2))
arma22

# Diagnostic Analysis
eth_close_rand1 <- eth_close_rand[200: 408]
is.na(eth_close_rand1)
r <- resid(arima(eth_close_rand1, order = c(2, 0, 2)))
is.na(r)
plot(r)
acf(r)

# Lag 11 shows the highest level of autocorelation in the residual

# Further exploration to do the forecasting and calculating the accuracy using RMSE
# with 90% data as training data and 10% as testing data
library(forecast, quietly = T)

eth_train <- eth_close_rand1[1:((0.9) * length(eth_close_rand1))]
eth_test <- eth_close_rand1[(0.9 * length(eth_close_rand1)) : length(eth_close_rand1)]
train11 <- arima(eth_train, order = c(2, 0, 2))
pred <- predict(train11, n.ahead = round(length(eth_close_rand1) - (0.9 * length(eth_close_rand1))))$pred
forecast <- forecast(train11, h = 25)
plot(forecast)

# Accuracy of the model with chosen p and q
accuracy(pred, eth_test)[2]*100

