############################################################################################
#Bitcoin_Price_Add_Missing_Values.R
#Bitcoin_Price.csv has following attributes:
#Date,Open,High,Low,Close,Volume,MarketCap

#However, Volume feature has missing values. This script uses Linear Regression model
#to fill the missing values. We are predicting Volume with respect to average daily price.
#Average Daily Price is calculated as:
#average_daily_price = (High + Low)/2.

#Average daily price gives the trader indication of which direction the price might move.
#And that may impact the decision of people trading on that average rate.

#Author: Nayna Jain
############################################################################################

#File bitcoin_price_normalized.csv has all the attributes normalized from bitcoin_price.csv
#Normalization is done using formula - (value - average)/standard deviation
bitcoin_price <- read.csv("/home/nayna/nayna/documents/iiitb/courses/da/project/datasets/cryptocurrencypricehistory/bitcoin_price_normalized.csv")

#Describes the data
str(bitcoin_price)
#'data.frame':	1620 obs. of  7 variables:
#$ Date     : Factor w/ 1620 levels "Apr 01, 2014",..: 1358 1353 1348 1620 1615 1610 1605 1600 1595 1590 ...
#$ Open     : num  4.39 4.37 4.31 4.1 4.11 ...
#$ High     : num  4.25 4.29 4.21 4.16 4 ...
#$ Low      : num  4.38 4.53 4.4 4.26 4.11 ...
#$ Close    : num  4.25 4.36 4.36 4.28 4.07 ...
#$ Volume   : num  2.06 2.34 1.9 1.9 2.21 ...
#$ MarketCap: num  4.42 4.4 4.34 4.13 4.13 ...

#Since Date has been read by the R as Factor, we have to convert it into proper date format.
bitcoint_price_date <- as.Date(bitcoint_price_date,"%B %d, %Y")
head(bitcoint_price_date)
#[1] "2017-10-03" "2017-10-02" "2017-10-01" "2017-09-30" "2017-09-29" "2017-09-28"

#Describes the date attribute
str(bitcoint_price_date)
#Date[1:1620], format: "2017-10-03" "2017-10-02" "2017-10-01" "2017-09-30" "2017-09-29" "2017-09-28" "2017-09-27" "2017-09-26" ...

#Reassign the updated Date Vector to data frame as below:
bitcoin_price$Date <- bitcoint_price_date

#Now again describe the data frame. And now it shows it as Date as we want.
str(bitcoin_price)

#'data.frame':	1620 obs. of  7 variables:
#$ Date     : Date, format: "2017-10-03" "2017-10-02" "2017-10-01" "2017-09-30" ...
#$ Open     : num  4.39 4.37 4.31 4.1 4.11 ...
#$ High     : num  4.25 4.29 4.21 4.16 4 ...
#$ Low      : num  4.38 4.53 4.4 4.26 4.11 ...
#$ Close    : num  4.25 4.36 4.36 4.28 4.07 ...
#$ Volume   : num  2.06 2.34 1.9 1.9 2.21 ...
#$ MarketCap: num  4.42 4.4 4.34 4.13 4.13 ...

#Next we calculate the average daily price using the formula (Low + High)/2.
low <- bitcoin_price$Low
high <- bitcoin_price$High
amean <- rowMeans(cbind(low,high),na.rm = TRUE)

#Add new vector average daily price to the bitcoin_price data frame.
bitcoin_price <- within(bitcoin_price, {average_daily_price <- amean})

#Create the linear regression model.
model <- lm(bitcoin_price$Volume ~ bitcoin_price$average_daily_price, data = bitcoin_price)

#Create a logical vector to mark the NA (missing values)in Volume.
I <- is.na(bitcoin_price$Volume)

#Predict the missing values and assign into the data frame Volume attribute.
bitcoin_price$Volume[I] <- predict(model, newdata = bitcoin_price[I,])
tail(bitcoin_price$Volume)

#Rewrite the updated data frame to the new csv file which has no more any missing values.
write.csv(bitcoin_price,"/home/nayna/nayna/documents/iiitb/courses/da/project/datasets/cryptocurrencypricehistory/bitcoin_price_clean.csv",row.names=FALSE)

