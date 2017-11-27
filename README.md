# DA_Cryptocurrency
Exploratory Analytics on Cryptocurrencies 

Our Dataset:

Top 10 cryptocurrencies
- https://www.kaggle.com/sudalairajkumar/cryptocurrencypricehistory

Other Similar Datasets:

For all cryptocurrencies
- https://www.kaggle.com/jessevent/all-crypto-currencies

Top 100 cryptocurrencies
- https://www.kaggle.com/natehenderson/top-100-cryptocurrency-historical-data/data


Expectations from the document:

- Business guidelines 
- Target users: Who is the target consumer of your analytics? Describe how Analytics is likely to help those target users.
- Business Benefits: The project will include 4 analytics milestones. Describe the potential business benefits of each of the milestones
    - Descriptive and Exploratory Analytics
    - Data mining - classification
    - Data mining - clustering
    - Data mining - association rules

Reading Literature:
- https://dealbook.nytimes.com/2014/01/21/why-bitcoin-matters/?_php=true&_type=blogs&_r=0
- http://www.dummies.com/programming/big-data/phase-1-of-the-crisp-dm-process-model-business-understanding/
- https://hackernoon.com/bitcoin-ethereum-blockchain-tokens-icos-why-should-anyone-care-890b868cec06
- https://medium.freecodecamp.org/blockchain-is-our-first-22nd-century-technology-d4ad45fca2ce
- https://www.linkedin.com/pulse/blockchain-absolute-beginners-mohit-mamoria/
- https://arxiv.org/pdf/1611.03941.pdf
- https://decentralize.today/5-benefits-of-cryptocurrency-a-new-economy-for-the-future-925747434103
- http://seoulai.com/presentations/Trading_Bitcoin_and_Online_Time_Series_Prediction.pdf
- http://ai2-s2-pdfs.s3.amazonaws.com/e065/3631b4a476abf5276a264f6bbff40b132061.pdf
- http://trap.ncirl.ie/2496/1/seanmcnally.pdf

For PCA we have referred the following links:
http://web.missouri.edu/~huangf/data/mvnotes/Documents/pca_in_r_2.html

https://rpubs.com/aaronsc32/eigenvalues-eigenvectors-r
https://stat.ethz.ch/R-manual/R-devel/library/stats/html/cor.html

https://www.analyticsvidhya.com/blog/2016/03/practical-guide-principal-component-analysis-python/

## Milestone 1 - Business Understanding

## Milestone 2

### Data Understanding

There are two types of datasets:

1. Related to daily trading on cryptocurrency. This includes:
Date, Low, High, Close, Open, Volume, MarketCap
All the data except Date is of numeric and continuous type.

2. Related to other attributes specific to particular cryptocurrency
Eg. bitcoin_dataset. These includes hash transactions, no of transaction
per block, block size. This type of data is available only for 
bitcoin and ethereum.

-Data Quality assessment
-Missing values prediction

Imputing missing data
- https://datascienceplus.com/imputing-missing-data-with-r-mice-package/
- https://www.analyticsvidhya.com/blog/2016/03/tutorial-powerful-packages-imputing-missing-values/
- https://cran.r-project.org/doc/contrib/de_Jonge+van_der_Loo-Introduction_to_data_cleaning_with_R.pdf

### Data Preparation
-Normalisation of the Bitcoin and Ethereum data

Dataset used is bitcoin_price.csv

Though all the features(attributes) are in numeric format except Date,
but the values in Volume/Market Capitalization are very high to use them
for computation. For that reason, the data is normalized for all the columns
to bring them to same scale.

To noramlize the data, following formula is used:
(value - average)/(standard deviation).

http://www.statisticshowto.com/normalized/
http://www.dataminingblog.com/standardization-vs-normalization/

Since the dataset has lot of outliers because of recent large surge in the prices,
z-score mechanism rather than (x - xmin)/(xmax-xmin).

Only the volume feature has missing values. There are multiple mechanims to handle
missing values eg:

1 Ignore the rows with missing values
2 Fill the missing values using mean/median
3 Use the regression and predict the missing values.

By opting 1, the data useful from other columns could have also been lost. And
option 2 was not useful because this dataset has many outliers and so using
option 2 could have been given biased values. So, option 3 is opted.
Linear Regression is used for continuous data and since the attribute Volume is
continuous, this model is used for prediction.

















