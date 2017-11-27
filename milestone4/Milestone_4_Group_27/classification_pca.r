# Loading the libraries
library(e1071) 
library(caret)
library(caTools)
library(readr)
library(utils)
library(randomForest)
library(nnet)

# Setting the directory
setwd('/home/akanksha/Desktop/Sem III/Data Analytics/Course Project/Dataset/cryptocurrencypricehistory/Cleaned Dataset')

# Load MODIFIED dataset
eth <- read_csv('eth_dataset_cleaned_filtered.csv')
eth$eth_price_binary_label <- as.factor(eth$eth_price_binary_label) # For binary label 
eth$eth_price_ternary_label <- as.factor(eth$eth_price_ternary_label) # For ternary label
str(eth)

### PRINCIPAL COMPONENT ANALYSIS
### PCA WITHOUT CONSIDERING DATE COLUMN

# Exclude date from the dataset to prepare it for pca
eth_without_date <- eth[, 2:17]

# PCA Application (only on  numeric variables (exclude response variables if any))
eth_pca <- prcomp(eth_without_date, retx = TRUE, scale = FALSE, center = FALSE)
#summary(eth_pca)

# Scree Plot
plot(eth_pca)

# Draw Elbow curve
plot(eth_pca, type ="l")

### Number of components
nComp <- 6

### Reconstructing original data
eth_pca_proj_data <- eth_pca$x[, 1:nComp] %*% t(eth_pca$rotation[1:nComp, 1:nComp])

### CLASSIFICATION ON ETHEREUM DATASET TO CLASSIFY ETHEREUM PRICE
### FOR BINARY LABEL

# Dataset with binary label
eth_pca_proj_binary <- as.data.frame(eth_pca_proj_data)
eth_pca_proj_binary <- cbind(eth_pca_proj_binary, eth_price_binary_label = eth$eth_price_binary_label)

### Splitting the dataset
set.seed(101)
sample_binary = sample.split(eth_pca_proj_binary,SplitRatio = .85)
train_binary = subset(eth_pca_proj_binary, sample_binary==TRUE)
test_binary = subset(eth_pca_proj_binary, sample_binary==FALSE)

### Building the models
model_multinom_binary <- multinom(eth_price_binary_label ~ ., data = train_binary)
model_rf_binary <- randomForest(eth_price_binary_label ~ ., data = train_binary, ntree = 100)
model_svm_binary <- svm(eth_price_binary_label ~ .,data = train_binary,kernel = 'polynomial' , gamma = 0.2, cost=100)

### Testing the model through predictions and building confusion matrices
predictions_multinom_binary <- predict(model_multinom_binary, test_binary[, 1:nComp])
#predictions_multinom
conMat_multinom_binary <- table(predictions_multinom_binary, test_binary$eth_price_binary_label)
confusionMatrix(conMat_multinom_binary)

predictions_rf_binary <- predict(model_rf_binary, test_binary[, 1:nComp])
#predictions_rf
conMat_rf_binary <- table(predictions_rf_binary, test_binary$eth_price_binary_label)
confusionMatrix(conMat_rf_binary)

predictions_svm_binary <- predict(model_svm_binary, test_binary[, 1:nComp])
#predictions_svm
conMat_svm_binary <- table(predictions_svm_binary, test_binary$eth_price_binary_label)
confusionMatrix(conMat_svm_binary)

### FOR TERNARY LABEL
# Dataset with binary label
eth_pca_proj_ternary <- as.data.frame(eth_pca_proj_data)
eth_pca_proj_ternary <- cbind(eth_pca_proj_ternary, eth_price_ternary_label = eth$eth_price_ternary_label)

### Splitting the dataset
sample_ternary = sample.split(eth_pca_proj_ternary, SplitRatio = .85)
train_ternary = subset(eth_pca_proj_ternary, sample_ternary ==TRUE)
test_ternary = subset(eth_pca_proj_ternary, sample_ternary ==FALSE)

### Building the models
model_multinom_ternary <- multinom(eth_price_ternary_label ~ ., data = train_ternary)
model_rf_ternary <- randomForest(eth_price_ternary_label ~ ., data = train_ternary, ntree = 100)
model_svm_ternary <- svm(eth_price_ternary_label ~ .,data = train_ternary, kernel = 'polynomial' , gamma = 0.2, cost=100)

### Testing the model through predictions and building confusion matrices
predictions_multinom_ternary <- predict(model_multinom_ternary, test_ternary[, 1:nComp])
#predictions_multinom
conMat_multinom_ternary <- table(predictions_multinom_ternary, test_ternary$eth_price_ternary_label)
confusionMatrix(conMat_multinom_ternary)

predictions_rf_ternary <- predict(model_rf_ternary, test_ternary[, 1:nComp])
#predictions_rf
conMat_rf_ternary <- table(predictions_rf_ternary, test_ternary$eth_price_ternary_label)
confusionMatrix(conMat_rf_ternary)

predictions_svm_ternary <- predict(model_svm_ternary, test_ternary[, 1:nComp])
#predictions_svm
conMat_svm_ternary <- table(predictions_svm_ternary, test_ternary$eth_price_ternary_label)
confusionMatrix(conMat_svm_ternary)


