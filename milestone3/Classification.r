
### AUTHORS - Akanksha Dwivedi - MT2016006
### Hitesha Mukherjee - MS2016007
### Nayna Jain - MS2017003
### Tarini Chandrashekhar - MT2016144 


library(e1071) 
library(caret)
library(caTools)
library(readr)
library(utils)
library(randomForest)
library(nnet)

### For Ternary Classification with classes as -1, 0, 1
### Loading the dataset
bit_dataset <- read_csv('C:/Users/Akanksha/Desktop/Sem III/Data Analytics/Course Project/Cleaned Dataset/bit_dataset_cleaned _filtered.csv')
bit_dataset$btc_market_price_label <- as.factor(bit_dataset$btc_market_price_label)
#str(bit_dataset)

### Splitting the dataset
set.seed(101)
sample = sample.split(bit_dataset,SplitRatio = .75)
train = subset(bit_dataset, sample==TRUE)
test = subset(bit_dataset, sample==FALSE)

### Training the different classification models
model_multinom <- multinom(btc_market_price_label ~ ., data = train)
model_svm <- svm(btc_market_price_label ~ .,data = train,kernel = 'polynomial' , gamma = 0.2, cost=100)
model_rf <- randomForest(btc_market_price_label ~ ., data = train, ntree = 60)
model_naivebayes <- naiveBayes(btc_market_price_label~., data = train)

### Testing the model through predictions and building confusion matrices
predictions_multinom <- predict(model_multinom, test[, 1:16])
#predictions_multinom
conMat_multinom <- table(predictions_multinom, test$btc_market_price_label)
confusionMatrix(conMat_multinom)

predictions_svm <- predict(model_svm, test[, 1:16])
#predictions_svm
conMat_svm <- table(predictions_svm, test$btc_market_price_label)
confusionMatrix(conMat_svm)

predictions_rf <- predict(model_rf, test[, 1:16])
#predictions_rf
conMat_rf <- table(predictions_rf, test$btc_market_price_label)
confusionMatrix(conMat_rf)

predictions_naivebayes <- predict(model_naivebayes, test[, 1:16])
#predictions_naivebayes
conMat_naivebayes <- table(predictions_naivebayes, test$btc_market_price_label)
confusionMatrix(conMat_naivebayes)

### For binary classification : -1, 1
### Loading the dataset
bit_dataset_binary <- read_csv('C:/Users/Akanksha/Desktop/Sem III/Data Analytics/Course Project/Cleaned Dataset/bit_dataset_cleaned _filtered_binary_label.csv')
bit_dataset_binary$btc_market_price_label <- as.factor(bit_dataset_binary$btc_market_price_label)

### Splitting the dataset
set.seed(101)
sample = sample.split(bit_dataset_binary,SplitRatio = .75)
train_binary = subset(bit_dataset_binary, sample==TRUE)
test_binary = subset(bit_dataset_binary, sample==FALSE)

### Training the different classification models
model_multinom_binary <- multinom(btc_market_price_label ~ ., data = train_binary)
model_svm_binary <- svm(btc_market_price_label ~ .,data = train_binary,kernel = 'polynomial' , gamma = 0.2, cost=100)
model_rf_binary <- randomForest(btc_market_price_label ~ ., data = train_binary, ntree = 100)
model_naivebayes_binary <- naiveBayes(btc_market_price_label~., data = train_binary)

### Testing the model through predictions and building confusion matrices
predictions_multinom_binary <- predict(model_multinom_binary, test_binary[, 1:16])
conMat_multinom_binary <- table(predictions_multinom_binary, test_binary$btc_market_price_label)
confusionMatrix(conMat_multinom_binary)

predictions_svm_binary <- predict(model_svm_binary, test_binary[, 1:16])
conMat_svm_binary <- table(predictions_svm_binary, test_binary$btc_market_price_label)
confusionMatrix(conMat_svm_binary)

predictions_rf_binary <- predict(model_rf_binary, test_binary[, 1:16])
conMat_rf_binary <- table(predictions_rf_binary, test_binary$btc_market_price_label)
confusionMatrix(conMat_rf_binary)

predictions_naivebayes_binary <- predict(model_naivebayes_binary, test_binary[, 1:16])
conMat_naivebayes_binary <- table(predictions_naivebayes_binary, test_binary$btc_market_price_label)
confusionMatrix(conMat_naivebayes_binary)
