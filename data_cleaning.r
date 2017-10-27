library(readr)
library(xlsx)
library(lubridate)
library(splitstackshape)

setwd("C:/Users/Akanksha/Desktop/Sem III/Data Analytics/Course Project/cryptocurrencypricehistory")

bitcoin_cash_price <- read_csv('bitcoin_cash_price.csv')
which(is.na(bitcoin_cash_price))

#bitcoin_dataset <- read_csv('bitcoin_dataset.csv')
#bitcoin_dataset$Date <- as.Date(bitcoin_dataset$Date, format= "%m/%d/%Y")

#bitcoin_price <- read_csv('bitcoin_price.csv')

ethereum_classic_price <- read_csv('ethereum_classic_price.csv')
which(is.na(ethereum_classic_price)) ### which indices have the missing values

ethereum_price <- read_csv('ethereum_price.csv')
ethereum_price$Date <- as.Date(ethereum_price$Date, format="%m/%d/%Y")
sum(is.na(ethereum_price))

ethereum_dataset <- read_csv('ethereum_dataset.csv')
ethereum_dataset$`Date(UTC)` <- as.Date(ethereum_dataset$`Date(UTC)`, format="%m/%d/%Y")
is.na(ethereum_dataset$`Date(UTC)`)
which(is.na(ethereum_dataset)) 
sum(is.na(ethereum_dataset))    ### total number of missing values
summary(ethereum_dataset)

### How many values of a particular features are missing? If more than 5% are missing,
### then the feature can be dropped or more measurements can be made for this feature.
miss_threshold <- sum(is.na(ethereum_dataset[, 18]))/nrow(ethereum_dataset[, 18])*100
miss_threshold  ### 81% are missing. Si this attribute will not help in learning from the data

### installing mice package for handling and imputing missing data
# install.packages('mice', dependencies=TRUE)
library(mice)

### FOR MISSING DATA AT RANDOM
### analysing the pattern of missing values in each column of data frame
md.pattern(ethereum_dataset)

### For visual representation of missing data
library(VIM)
aggr_plot <- aggr(ethereum_dataset, col=c('navyblue', 'red'), numbers= TRUE, sortVars = TRUE, labels=names(ethereum_dataset), cex.axis=.7, gap=3, ylab=c("Histogram of missing data", "Pattern"))
### Incorrect results in the plot

marginplot(ethereum_dataset[c(1,18)])  ### If missing data at random then blue and red box plots would have been similar

### Imputing the missing data
md.pattern(ethereum_price)
str(ethereum_price)
temp1 <- subset(ethereum_price, select = -c(Date))

full_eth_price_data <- mice(temp1, m=5, maxit=25, meth='pmm', seed=500) ### Not working correctly
