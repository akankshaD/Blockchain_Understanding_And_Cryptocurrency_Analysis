library(caret)
library(readr)
library(glmnet)
library(tictoc)

# Setting the directory
setwd('/home/akanksha/Desktop/Sem III/Data Analytics/Course Project/Dataset/cryptocurrencypricehistory/Cleaned Dataset')

# Load MODIFIED dataset
eth <- read_csv('eth_dataset_cleaned_filtered.csv')

# Drop eth_price_ternary_label and Date(UTC) column 
eth <- within(eth, rm("eth_price_ternary_label", "Date(UTC)"))
#str(eth)
eth$eth_price_binary_label <- as.factor(eth$eth_price_binary_label) # For binary label 
#str(eth)

# Create random data partitions into train and test sets

# Filtering the label column and convert it into a vector
classes <- eth[, "eth_price_binary_label"]
classes_vec <- as.numeric(classes$eth_price_binary_label)

# Creating random test/train partitions
train_set <- createDataPartition(classes_vec, p=0.8, list=FALSE)
str(train_set)

set.seed(120)
cv_splits <- createFolds(classes_vec, k=10, returnTrain = TRUE)
str(cv_splits)

nPca <- 6
eth_pca <- eth[, 1:6]
eth_pca <- cbind(eth_pca, eth_price_binary_label = eth$eth_price_binary_label)

### Fitting the Generalized Linear Model using 10 fold cross validation
eth_train <- eth_pca[train_set, ]
eth_test <- eth_pca[-train_set, ]

# Form a set of combinations of parameter lambda and alpha for tuning the model
glmnet_grid <- expand.grid(alpha = c(0, .1, .2, .4, .6, .8, 1), lambda = seq(.01, .2, length = 20))

glmnet_cntrl <- trainControl(method = "LOOCV", number = 10)

tic()
glmnet_fit <- train(eth_price_binary_label ~ ., data = eth_train, method = "glmnet", tuneGrid = glmnet_grid, trControl = glmnet_cntrl)
toc()

glmnet_fit 
# Training accuracy was used to select the optimal model using  the largest value which was found to be 54.8%.
# The final values used for the model were alpha = 0.1 and lambda = 0.01.

### Testing the model through predictions and building confusion matrices
pred_classes <- predict(glmnet_fit, eth_test[, 1:nPca])
conMat <- table(pred_classes, eth_test$eth_price_binary_label)
confusionMatrix(conMat)

# Testing Accuracy was found to be 52.2%

# Visualization of tuning parameters
trellis.par.set(caretTheme())
plot(glmnet_fit, scales = list(x = list(log = 2)))

# The plot shows the “accuracy”, that is the percentage of correctly classified observations,
# for the penalized logistic regression model with each combination of the two tuning parameters