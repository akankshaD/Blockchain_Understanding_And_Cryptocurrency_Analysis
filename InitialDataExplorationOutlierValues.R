#   Initial Data Exploration and Detecting Outlier Values 
#AUTHOR - Hitesha Mukherjee(MS2016007)

setwd() # setting your file directory
getwd() # getting your file directory
#XLConnect, gdata package can be also be used for loading excel, csv etc.

# Identifying Missing Values in bitcoin_price.csv

library(xlsx)
# 1) Reading bitcoin_price.csv
library(readr)
bitcoin_price <- read_csv("your path to csv")

# 2) Finding out the indices missing values from the volume attribute

missingdata <- which(is.na(bitcoin_price[[6]]))

# 3) Finding out the total number of missing values (NA) in the dataset.
total_missed <- sum(is.na(bitcoin_price))
total_missed
#if you call  total_missed explicity we observe 243 values are missing in overall only in volume column 
# rest of the columns are fine.


#bitcoin_dataset.csv - Found Outliers in it and did boxplot for identifying them
# No missing values found in bitcoin_dataset.csv

# 4loading the csv
df <- read.csv(file.path("path of the folder where the files are present", "bitcoin_dataset.csv"))
  
#counting how many rows are there in the given data
nrows = nrow(df)
nrows

#The function complete.cases() on a data frame returns TRUE for every row where there
#are no NAs, FALSE otherwise
ncomplete  = sum(complete.cases(df))
ncomplete

# 5 Bivariate approach for Visualize in box-plot of the X and Y, for categorical X's ... For continuous variable which is converted to categorical
# for two columns(attributes comparison in this case btc_total_bitcoins and btc_avg_block_size) ). We observe outliers which show up as dots.
boxplot(btc_total_bitcoins ~ cut(btc_avg_block_size, pretty(df$btc_avg_block_size)), data=df, main="Boxplot for btc_total_bitcoins vs avg_block _size (categorial) approach", cex.axis=0.5)

# 6 Bivariate approach for Visualize in box-plot of the X and Y, for categorical X's ... For continuous variable which is converted to categorical
# for two columns(attributes comparison in this case btc_market_cap and btc_market_price) ). We observe outliers which show up as dots.
boxplot(btc_market_cap ~ cut(btc_market_price, pretty(df$btc_market_price)), data=df, main="Boxplot for BitCoin (Market Cap and MarketPrice", cex.axis=0.5)


# 7 Univariate Approach(for marking outliers on single column(attribute))For a given continuous variable, outliers are those observations that lie outside 1.5 * IQR, where IQR, the 'Inter Quartile Range' 
#is the difference between 75th and 25th quartiles. Look at the points outside the whiskers in below box plot.We observe outliers which show up as dots.
outlier_values <- boxplot.stats(df$btc_market_price)$out 
boxplot(df$btc_market_price, main="MarketPrice", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)

# 8 similar as above for avg_block_size column or attribute, We observe outliers which show up as dots.
outlier_values <- boxplot.stats(df$btc_avg_block_size)$out 
boxplot(df$btc_avg_block_size, main="MarketPrice", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)

#9 Below command is useful for generating plots for multiple columns of csv and can be visualized
#in one single graph, this gives non-coloured output
library(ggplot2)
df <- data.frame(time = 1:10,
btc_hash_rate = cumsum(rnorm(10)),
btc_total_bitcoins = cumsum(rnorm(10)),
btc_avg_block_size = cumsum(rnorm(10)),
btc_market_price = cumsum(rnorm(10)),
btc_market_cap = cumsum(rnorm(10)),
btc_trade_volume = cumsum(rnorm(10)),
btc_n_orphaned_blocks = cumsum(rnorm(10)),
btc_n_transactions_per_block = cumsum(rnorm(10)),
btc_median_confirmation_time= cumsum(rnorm(10)))
df <- melt(df ,  id.vars = 'time', variable.name = 'series')
ggplot(df, aes(time,value)) + geom_line() + facet_grid(series ~ .)

# 10 Below is the command in ggplot2 package for displaying graphs in colour

df <- data.frame(time = 1:10,
      btc_total_bitcoins = cumsum(rnorm(10)),
      btc_avg_block_size = cumsum(rnorm(10)),
      btc_hash_rate = cumsum(rnorm(10)))
df <- melt(df ,  id.vars = 'time', variable.name = 'series')
ggplot(df, aes(time,value)) + geom_line(aes(colour = series)) + facet_grid(series ~ .)

# 11 Check for bad or missing values
#Bad values can be missing (NA) or problematic types (NaN, Inf). They can also be invalid values: invalid
#category levels, implausible numeric values (negative ages; values that are outside the range a variable
#would be expected to take). Identifying bad values often requires domain knowledge of the plausible
#values of the variables.A special kind of bad numerical value is the sentinel value: a value that used to represent "unknown" or "not
#applicable" or other special cases in numeric data. A -1 in age data can be a sentinel value, meaning "age
#unknown." All 9s is also a common sentinel value. Sometimes, the maximum value of a variable may really
#be the maximum recorded value; any value greater than that maximum was censored down.

#Detecting sentinel values
#One way to detect sentinel values is to look for sudden jumps in an otherwise smooth distribution of
#values.

sentinal =90000
isbad = which(df$btc_hash_rate == sentinal)
length(isbad)

sentinal =90000
isbad = which(df$btc_avg_block_size == sentinal)
length(isbad)

  