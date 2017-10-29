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

### Plots for ethereum_classic_price
aggr_plot_eth_classic_price <- aggr(eth_classic_price, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(eth_classic_price), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
### boxplot for two attributes
marginplot(eth_classic_price[c(1,7)])



eth_price <- read_csv('ethereum_price.csv')
eth_price$Date <- as.Date(eth_price$Date, format="%B %d,%Y")

### Plots for ethereum_price
aggr_plot_eth_price <- aggr(eth_price, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(eth_price), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
### boxplot for two attributes
marginplot(eth_price[c(1,7)])



eth_dataset <- read_csv('ethereum_dataset.csv')
eth_dataset$`Date(UTC)` <- as.Date(eth_dataset$`Date(UTC)`, format="%m/%d/%Y")

### Plots for ethereum_dataset
aggr_plot_eth_dataset <- aggr(eth_dataset, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(eth_dataset), cex.axis=.4, gap=1, ylab=c("Histogram of missing data","Pattern"))
#sum(is.na(eth_dataset$`Date(UTC)`))
### boxplot for two attributes
marginplot(eth_dataset[c(1,18)])



bit_price <- read.csv('bitcoin_price.csv')
bit_price$Date <- as.Date(bit_price$Date, format = "%B %d,%Y")

### Plots for ethereum_dataset
aggr_plot_bit_price <- aggr(bit_price, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(bit_price), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
#sum(is.na(eth_dataset$`Date(UTC)`))
### boxplot for two attributes
marginplot(bit_price[c(1,6)])

bit_cash_price <- read.csv('bitcoin_cash_price.csv')
bit_cash_price$Date <- as.Date(bit_cash_price$Date, format = "%B %d,%Y")

### Plots for ethereum_dataset
aggr_plot_bit_cash_price <- aggr(bit_cash_price, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(bit_cash_price), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
#sum(is.na(eth_dataset$`Date(UTC)`))
### boxplot for two attributes
marginplot(bit_cash_price[c(1,7)])


bit_dataset <- read.csv('bitcoin_dataset.csv')
bit_dataset$Date <- as.Date(bit_dataset$Date, format = "%Y-%m-%d %H:%M:%S")

### Plots for ethereum_dataset
aggr_plot_bit_dataset <- aggr(bit_dataset, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(bit_dataset), cex.axis=.3, gap=1, ylab=c("Histogram of missing data","Pattern"))
#sum(is.na(eth_dataset$`Date(UTC)`))
### boxplot for two attributes
marginplot(bit_dataset[c(1,5)])
