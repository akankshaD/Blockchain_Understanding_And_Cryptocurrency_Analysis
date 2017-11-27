library(marima)
library(readr)

rm(list=ls())

# Setting the directory
setwd('/home/akanksha/Desktop/Sem III/Data Analytics/Course Project/Dataset/cryptocurrencypricehistory')

# Load dataset
eth <- read_csv('ethereum_price.csv')
eth <- eth[seq(dim(eth)[1], 1), ]    # reversing the dataframe
eth <- eth[2:dim(eth)[1], ]

# Differencing the dataframe to stabilize the variance of the dat
Open <- diff(log(eth$Open))
Close <- diff(log(eth$Close))
Open <- diff(log(eth$Open))
High <- diff(log(eth$High))
Low <- diff(log(eth$Low))
Volume <- diff(log(eth$Volume))
MarketCap <- diff(log(eth$`Market Cap`))

eth_mod <- data.frame(Date=eth$Date[2:dim(eth)[1]], Open, High, Low, Close, Volume, MarketCap)

# Converting the data frame into a time series object
eth_modified <- ts(eth_mod)

# Plotting the data
plot(eth_modified)

# Transposing the matrix and selecting the data (1400 rows) for training
eth_transp <- t(eth_modified)[, 1:787]

# Defining MARIMA model
# kvar = # of variables in the data

# ar = to define windows (p) for auto regression
# ma = to define windows (q) for auto regression
# rem.var = variables to be removed for analysis
# reg.var = regression variables

eth_model <- define.model(kvar = 7, ar= 2, ma = 2, rem.var = c(1), reg.var = c(7)) 

# Estimate MARIMA model
eth_marima <- marima(eth_transp, eth_model$ar.pattern, eth_model$ma.pattern, penalty = 1)

# Print estimates
short.form(eth_marima$ar.estimates, leading = FALSE)
short.form(eth_marima$ma.estimates, leading = FALSE)
round(eth_marima$resid.cov) # Covariance matrix of residuals
round(eth_marima$data.cov) # Covariance matrix of original variables
eth_marima$ar.fvalues   # gives significance of estimated coefficients by means of f values
eth_marima$ma.fvalues

# Calculation of residuals and filtered values of timeseries using arima model
residual <- arma.filter(eth_transp, eth_marima$ar.estimates, eth_marima$ma.estimates, means = 1)

# Comparison of residuals
plot(eth_marima$residuals, residual$residuals, xlab = 'Marima Residuals', ylab = 'arma.filter Residuals')

### Forecasting using Marima
# nstart : starting point for forecasting 
# nstep : length of forecast
nstart <- 770
nstep <- 17
forecasts <- arma.forecast(series = t(eth_modified), marima = eth_marima, nstart = nstart, nstep = nstep)

# Plot forecasting results
plot(eth_modified[1:787, 1], eth_modified[1:787, 7], type = 'l', col = 'black', xlab = 'Time Object of Time Series', ylab = 'Stabilized Close Values', main='Prediction Of Close Values', ylim = c(-2, 2))
lines(eth_modified[1:787, 1], forecasts$forecasts[7, ], type = 'l', col = 'green')
  

