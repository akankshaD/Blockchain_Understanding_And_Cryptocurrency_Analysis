############################################################################
# Dataset: bitcoin_dataset.csv
# Time Series Modelling: Univariate Analysis
# Attribute: Close Price
# Author: Nayna Jain (MS2017003)
#############################################################################


library('ggplot2')
library('forecast')
library('tseries')

#Explore the data
daily_data = read.csv('/home/nayna/nayna/documents/iiitb/courses/da/project/datasets/cleaned_dataset/bitcoin_price_new_volume_filled.csv', header=TRUE, stringsAsFactors=FALSE)
str(daily_data)
daily_data$Date = (as.Date(daily_data$Date, "%B %d, %Y"))
str(daily_data)
ggplot(daily_data, aes(Date, Open)) + geom_line() + scale_x_date('Date')  + ylab("Open Price") +  xlab("")
close_ts = ts(daily_data[,c('Close')])

#Attribute to be analysed has to be reversed so that time series starts from the past and has highest count for the latest.
rev_close_ts = rev(ts(daily_data[, c('Close')]))

#Since we reversed, it now plots from 2013 to 2017 time frame.
ggplot() + geom_line(data = daily_data, aes(x = rev(Date), y = rev(daily_data$Close))) + ylab('Close Price')

#This cleans the outliers and missing values. Outliers are cleaned so that there is no skewing of data.
rev_clean_close = tsclean(rev_close_ts)

ggplot() + geom_line(data = daily_data, aes(x = rev(Date), y = tsclean(rev(daily_data$Close)))) + ylab('Close Price')

#Calculates weekly and monthly moving average. This moving average is different from MA(q) used in ARIMA modelling, as it i
#used for smoothening of the data.
rev_close_ma = ma(rev_clean_close, order = 7)
rev_close_ma30 = ma(rev_clean_close, order = 30)
rev_clean_close_ma = tsclean(rev_close_ma)
rev_clean_close_ma30 = tsclean(rev_close_ma30)

ggplot() +
  geom_line(data = daily_data, aes(x = rev(Date), y = tsclean(rev(daily_data$Close)))) + ylab('Close Price') +
  geom_line(data = daily_data, aes(x = rev(Date), y = rev_clean_close_ma)) + ylab('Close Price') +
  geom_line(data = daily_data, aes(x = rev(Date), y = rev_clean_close_ma30)) + ylab('Close Price')


#Identifies the trend components and plots
rev_close_new_ma = ts(na.omit(rev_close_ma), frequency = 30)
rev_decomp = stl(rev_close_new_ma, s.window="periodic")

#This is to remove the seasonal component.
rev_deseasonal_close <- seasadj(decomp)

plot(rev_decomp)
plot(rev_deseasonal_close)

#Stationary or Non-Stationary.
#We use Dickey Fullter Test
adf.test(rev_close_new_ma, alternative = "stationary")
#Result
#Augmented Dickey-Fuller Test
#
#data:  rev_close_new_ma
#Dickey-Fuller = 2.8463, Lag order = 11, p-value = 0.99
#p-value > 0.05 so, is not stationary.
#alternative hypothesis: stationary

plot(rev_close_new_ma)

#Since data is not found stationary, we start with differencing.
#Differencing by order 1.
rev_close_dl1 = diff(rev_deseasonal_close, differences = 1)
adf.test(rev_close_dl1, alternative = "stationary")

# Result
# Augmented Dickey-Fuller Test
#
# data:  rev_close_dl1
# Dickey-Fuller = -7.0292, Lag order = 11, p-value = 0.01
# alternative hypothesis: stationary
# Since p-value < 0.05, it is now stationary.
#
#Plot still has some higher range above mean, so we will try with differencing = 2 also.
plot(rev_close_dl1)

#Trying with differencing = 2
rev_close_dl2 = diff(rev_deseasonal_close, differences = 2)
adf.test(rev_close_dl2, alternative="stationary" )

# Result
# Augmented Dickey-Fuller Test
#
# data:  rev_close_dl2
# Dickey-Fuller = -13.746, Lag order = 11, p-value = 0.01
# alternative hypothesis: stationary
# Since p-value < 0.05, it is stationary data.
#
# Plot also now shows the mean as constant.
plot(rev_close_dl2)


# Plotting ACF and PACF to identify p and q parameters.
# Plotting with differencing = 0 data.
acf(rev_close_new_ma, main='ACF')
pacf(rev_close_new_ma, main='PACF')

# Plotting with differencing = 1 data.
acf(rev_close_dl1, main='ACF')
pacf(rev_close_dl1, main='PACF')

# Plotting with differencing = 2 data.
acf(rev_close_dl2, main='ACF')
pacf(rev_clsoe_dl2, main='PACF')

#Generating and fitting the model
#Tried first with p = 3, d = 2, q = 2.
fit <- arima(rev_deseasonal_close, order = c(3,2,2))
tsdisplay(residuals(fit), lag.max = 45, main='(3,2,2) Model Residuals')

#We see that lag = 7 seems to be a better choice. So, now we have p = 3, d = 2, q = 7.
fit2 <- arima(rev_deseasonal_close, order = c(3,2,7))
tsdisplay(residuals(fit2), lag.max = 45, main='(3,2,7) Model Residuals')

#Forecasting using the above generated model having h = 25 values.
fcast <- forecast(fit2, h=25)
plot(fcast)

#Forecasting using the above generated model having h = 100 values.
fcast <- forecast(fit2, h=100)
plot(fcast)

#Forecasting on using the part of the existing dataset and comparing with original data.
hold <- window(rev_deseasonal_close, start=700)
fit_no_holdout = arima(ts(rev_deseasonal_close[-c(700:725)]), order = c(3,2,7))
fcast_no_holdout <- forecast(fit_no_holdout, h=25)
plot(fcast_no_holdout, main=" ")
lines(ts(rev_deseasonal_close))

#For future predictions.
pred<- perdict(fit2, n.ahead= 10*12)
ts.plot(rev_deseasonal_close, 2.718^pred$pred, log = "y", lty=c(1,3))

