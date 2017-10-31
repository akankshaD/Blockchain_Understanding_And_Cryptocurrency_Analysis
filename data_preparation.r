### Loading the libraries
library(readr)
library(mice)
library(caret)
library(tibble)

### Setting the path for current working directory
setwd('/home/akanksha/Desktop/Sem III/Data Analytics/Course Project/Dataset/cryptocurrencypricehistory/')

### Loading the data
eth_classic_price <- read_csv('ethereum_classic_price.csv')
#str(eth_classic_price)

### To convert the date into proper format 
eth_classic_price$Date <- as.Date(eth_classic_price$Date, format= "%B %d,%Y")
sum(is.na(eth_classic_price))    #### 1 missing value in Market Cap attribute
md.pattern(eth_classic_price)    ### to know the pattern of missing values in their repective attributes

### Imputing the missing values
#View(eth_classic_price)
eth_temp <- mice(eth_classic_price[,2:7], m=5, maxit=25, meth='cart', seed=500)
#summary(eth_temp)

# To view the predicted value
#eth_temp$imp$`Market Cap`

### Merging missing values in the original dataset
eth_classic_price_full <- complete(eth_temp, 4)  ### feeding the value from 4th iterated dataset
#str(eth_classic_price_full)

### Normalizing the dataset
preObj <- preProcess(eth_classic_price_full[,1:6], method=c("center", "scale"))
eth_classic_price_cleaned <- predict(preObj, eth_classic_price_full[, 1:6])
#eth_classic_price_cleaned

### Adding the Date attribute
eth_classic_price_cleaned$Date <- eth_classic_price$Date

### Reordering the columns
eth_classic_price_cleaned <- eth_classic_price_cleaned[, c("Date", "Open", "High", "Low", "Close", "Volume", "Market Cap")]
#eth_classic_price_cleaned

### Writing the cleaned and normalized dataset
md.pattern(eth_classic_price_cleaned)
write.csv(eth_classic_price_cleaned, file="eth_classic_price_cleaned.csv", row.names = FALSE)



### REPEATING THE IMPUTING AND NORMALIZING PROCESS FOR OTHER FILES
### FOR ETEHEREUM_PRICE
eth_price <- read_csv('ethereum_price.csv')
#str(eth_price)
eth_price$Date <- as.Date(eth_price$Date, format="%B %d,%Y")
sum(is.na(eth_price))                ### 1 missing value in Market Cap attribute
md.pattern(eth_price)

### Imputing the missing values
#View(eth_price)
eth_temp <- mice(eth_price[,2:7], m=5, maxit=25, meth='cart', seed=500)
#summary(eth_temp)
#eth_temp$imp$`Market Cap`

### Merging missing values in the original dataset
eth_price_full <- complete(eth_temp, 4)  ### feeding the value from 4th dataset
#eth_price_full

### Normalizing the dataset
preObj <- preProcess(eth_price_full[,1:6], method=c("center", "scale"))
eth_price_cleaned <- predict(preObj, eth_price_full[, 1:6])

### Adding the date attribute in the data frame
eth_price_cleaned$Date <- eth_price$Date

eth_price_cleaned <- eth_price_cleaned[, c("Date", "Open", "High", "Low", "Close", "Volume", "Market Cap")]
#eth_price_cleaned

### Writing the cleaned and normalized dataset
md.pattern(eth_price_cleaned)
write.csv(eth_price_cleaned, file="eth_price_cleaned.csv", row.names = FALSE)



### FOR ETHEREUM_DATASET
eth_dataset <- read_csv('ethereum_dataset.csv')
#str(eth_dataset)
eth_dataset$`Date(UTC)` <- as.Date(eth_dataset$`Date(UTC)`, format="%m/%d/%Y")
sum(is.na(eth_dataset))              ### 644 missing values in eth_ens_register attribute
md.pattern(eth_dataset)

### Imputing the missing values
#View(eth_dataset)
eth_temp <- mice(eth_dataset[,2:18], m=5, maxit=25, meth='cart', seed=500)
#summary(eth_temp)
eth_temp$imp$eth_ens_register

### Merging missing values in the original dataset
eth_dataset_full <- complete(eth_temp, 4)  ### feeding the value from 4th dataset
#eth_dataset_full

### Normalizing the dataset
preObj <- preProcess(eth_dataset_full[,1:17], method=c("center", "scale"))
eth_dataset_cleaned <- predict(preObj, eth_dataset_full[, 1:17])

eth_dataset_cleaned$`Date(UTC)` <- eth_dataset$`Date(UTC)`
#str(eth_dataset_cleaned)

### Writing the cleaned and normalized dataset
md.pattern(eth_dataset_cleaned)
write.csv(eth_dataset_cleaned, file="eth_dataset_cleaned.csv", row.names = FALSE)



### FOR BITCOIN_PRICE
bit_price <- read.csv('bitcoin_price.csv')
#str(bit_price)
#View(bit_price)
bit_price$Date <- as.Date(bit_price$Date, format = "%B %d,%Y")
sum(is.na(bit_price))               ### 243 missing values in Volume attribute
md.pattern(bit_price)

### Imputing the missing values
#View(bit_price)
bit_temp <- mice(bit_price[,2:7], m=5, maxit=25, meth='cart', seed=500)
#summary(bit_temp)

### Merging missing values in the original dataset
bit_price_full <- complete(bit_temp, 4)  ### feeding the value from 4th dataset
#bit_price_full

### Normalizing the dataset
preObj <- preProcess(bit_price_full[,1:6], method=c("center", "scale"))
bit_price_cleaned <- predict(preObj, bit_price_full[, 1:6])

bit_price_cleaned$Date <- bit_price$Date
#str(bit_price_cleaned)

bit_price_cleaned <- bit_price_cleaned[, c("Date", "Open", "High", "Low", "Close", "Volume", "Market.Cap")]

### Writing the cleaned and normalized dataset
md.pattern(bit_price_cleaned)
write.csv(bit_price_cleaned, file="bit_price_cleaned.csv", row.names = FALSE)



### FOR BITCOIN_CASH_PRICE
bit_cash_price <- read.csv('bitcoin_cash_price.csv')
#str(bit_cash_price)
bit_cash_price$Date <- as.Date(bit_cash_price$Date, format = "%B %d,%Y")
sum(is.na(bit_cash_price))         ### 10 missing values in Market Cap attribute
md.pattern(bit_cash_price)

### Imputing the missing values
#View(bit_cash_price)
bit_temp <- mice(bit_cash_price[,2:7], m=5, maxit=25, meth='cart', seed=500)
#summary(bit_temp)

### Merging missing values in the original dataset
bit_cash_price_full <- complete(bit_temp, 4)  ### feeding the value from 4th dataset
#bit_cash_price_full

### Normalizing the dataset
preObj <- preProcess(bit_cash_price_full[,1:6], method=c("center", "scale"))
bit_cash_price_cleaned <- predict(preObj, bit_cash_price_full[, 1:6])

bit_cash_price_cleaned$Date <- bit_cash_price$Date

bit_cash_price_cleaned <- bit_cash_price_cleaned[, c("Date", "Open", "High", "Low", "Close", "Volume", "Market.Cap")]

### Writing the cleaned and normalized dataset
md.pattern(bit_cash_price_cleaned)
write.csv(bit_cash_price_cleaned, file="bit_cash_price_cleaned.csv", row.names = FALSE)



### FOR BITCOIN_DATASET
bit_dataset <- read.csv('bitcoin_dataset.csv')
#str(bit_dataset)
bit_dataset$Date <- as.Date(bit_dataset$Date, format = "%Y-%m-%d %H:%M:%S")
sum(is.na(bit_dataset))            ### 21 missing values in btc_trade_volume attribute
md.pattern(bit_dataset)

### Imputing the missing values
#View(bit_dataset)
bit_temp <- mice(bit_dataset[,2:24], m=5, maxit=25, meth='cart', seed=500)
#summary(bit_temp)

### Merging missing values in the original dataset
bit_dataset_full <- complete(bit_temp, 4)  ### feeding the value from 4th dataset
#bit_dataset_full

### Normalizing the dataset
preObj <- preProcess(bit_dataset_full[,1:23], method=c("center", "scale"))
bit_dataset_cleaned <- predict(preObj, bit_dataset_full[, 1:23])

bit_dataset_cleaned$Date <- bit_dataset$Date

#str(bit_dataset_cleaned)

### Writing the cleaned and normalized dataset
md.pattern(bit_dataset_cleaned)
write.csv(bit_dataset_cleaned, file="bit_dataset_cleaned.csv", row.names = FALSE)