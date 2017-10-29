### For Data Visualzation

### Loading the libraries
library(VIM)
library(readr)
library(mice)

### Setting the path for current working directory
setwd('/home/akanksha/Desktop/Sem III/Data Analytics/Course Project/Dataset/cryptocurrencypricehistory/')

### Loading the datasets
eth_classic_price <- read_csv('ethereum_classic_price.csv')
eth_classic_price$Date <- as.Date(eth_classic_price$Date, format= "%B %d,%Y")

aggr_plot_eth_classic_price <- aggr(eth_classic_price, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(eth_classic_price), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

eth_price <- read_csv('ethereum_price.csv')
eth_price$Date <- as.Date(eth_price$Date, format="%B %d,%Y")

eth_dataset <- read_csv('ethereum_dataset.csv')
eth_dataset$`Date(UTC)` <- as.Date(eth_dataset$`Date(UTC)`, format="%m/%d/%Y")

bit_price <- read.csv('bitcoin_price.csv')
bit_price$Date <- as.Date(bit_price$Date, format = "%B %d,%Y")

bit_cash_price <- read.csv('bitcoin_cash_price.csv')
bit_cash_price$Date <- as.Date(bit_cash_price$Date, format = "%B %d,%Y")

bit_dataset <- read.csv('bitcoin_dataset.csv')
bit_dataset$Date <- as.Date(bit_dataset$Date, format = "%Y-%m-%d %H:%M:%S")

