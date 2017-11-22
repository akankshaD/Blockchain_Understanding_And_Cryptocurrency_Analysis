#PCA 
library(readr)
library(dplyr)
library(ggplot2)
setwd("/Users/tarinichandra/Desktop/DA")

bit_dataset_cleaned <- read_csv("bit_dataset_cleaned.csv")

data <-bit_dataset_cleaned[,2:24]

data
#log.data <- log(data[,1:4])
data.pca <- prcomp(data,center = TRUE,scale. = TRUE, retx = TRUE)
summary(data.pca)
#class.color <- c(rep(2,100),rep(3,100))
#plot(data.pca, col = class.color, main = "sample on new axis")
##Reconstructing data from Principal components
nComp = 2
orig = data.pca$x[,1:nComp] %*% t(data.pca$rotation[1:2,1:2])
orig[10,]




#print(data.pca)
#summary(data.pca)
#An Elbow plot
plot(data.pca,type="l")
#SCri plot
plot(data.pca)
require(caret)
#trans <- preProcess(data[,1:4],method=c("BoxCox","center","scale","pca"))
#PC = predict(trans,data[,1:4])
#head(PC,3)
#trans$rotation
