> library("class")
> library("caTools")
> library("utils")
> library("caret")
> library("readr")
> library("dbscan")
> library("fpc")
> library("factoextra")
> library("fpc")
> bit_dataset_cleaned_filtered <- read_csv("C:/Users/hits/Downloads/DA_Cryptocurrency-master/milestone3/bit_dataset_cleaned _filtered.csv")
> View(bit_dataset_cleaned_filtered)

                   # K Nearest Neighbours Algorithm for Classification to determine k value to be used for DBScan Algorithm.
>set.seed(77)
> intrain <- createDataPartition(y = bit_dataset_cleaned_filtered$btc_market_price_label, p= 0.7, list = FALSE) # Create Partition by spiltting the data 75% for Training and rest for Testing
> training <- dat[intrain,] # Create Training Set
> testing <- dat[-intrain,] # Create Test Set
> bit_dataset_cleaned_filtered$btc_market_price_label <- as.factor(bit_dataset_cleaned_filtered$btc_market_price_label)
> trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3) # KNN Algorithm 10 folds with repeatition of 3 Times


> set.seed(110)
> knn_fit <- train(btc_market_price_label ~., data = training, method = "knn",
+                  trControl=trctrl,
+                  preProcess = c("center", "scale"),
+                  tuneLength = 10) # Applying KNN Fit on  Training DataSet
> knn_fit

# The Number of Samples obtained

# Below is the output Obtained..
k-Nearest Neighbors 

2045 samples
  16 predictor

Pre-processing: centered (16), scaled (16) 
Resampling: Cross-Validated (10 fold, repeated 3 times) 
Summary of sample sizes: 1840, 1840, 1841, 1841, 1840, 1840, ... 
Resampling results across tuning parameters:

  k   RMSE       Rsquared  
   5  0.9876103  0.02219333
   7  0.9724025  0.01800860
   9  0.9597795  0.01981933
  11  0.9523200  0.02057122
  13  0.9487404  0.02073406
  15  0.9442313  0.02250378
  17  0.9405527  0.02441565
  19  0.9374679  0.02647467
  21  0.9353324  0.02740052
  23  0.9338722  0.02823808

#RMSE was used to select the optimal model using  the smallest value.The final value used for the model was k = 23. The minimum value of K obtained is 5.
> plot(knn_fit)
> test_pred <- predict(knn_fit, newdata = testing) # Predict for Testing based on KNN Fit
> dbscan::kNNdistplot(bit_dataset_cleaned_filtered, k =  5) abline(h = 0.15, lty = 2) # We have used kNNdistplot to determine the minimum epsilon value
       
	                       # DBScan Algorithm - Partioning around Density
						   
						   
# Using differnt Values for Eps(Epsilon)
> set.seed(123)
> db <- fpc::dbscan(bit_dataset_cleaned_filtered, eps = 1, MinPts = 5)
> fviz_cluster(db, data = bit_dataset_cleaned_filtered, stand = FALSE,
+              ellipse = FALSE, show.clust.cent = FALSE,
+              geom = "point",palette = "jco", ggtheme = theme_classic()) # For Visualizing the Plots based on eps and MinPts

> set.seed(123)
> db <- fpc::dbscan(bit_dataset_cleaned_filtered, eps = 1.5, MinPts = 5)
> fviz_cluster(db, data = bit_dataset_cleaned_filtered, stand = FALSE,
+              ellipse = FALSE, show.clust.cent = FALSE,
+              geom = "point",palette = "jco", ggtheme = theme_classic())


> set.seed(123)
> db <- fpc::dbscan(training, eps = 1.5, MinPts = 5)
> fviz_cluster(db, data = training, stand = FALSE,
+              ellipse = FALSE, show.clust.cent = FALSE,
+              geom = "point",palette = "jco", ggtheme = theme_classic())

# Prediction based on Trainning and Testing . # Plotting based on it


> pred <- predict.dbscan(db, training, testing)
> plot(pred)
> set.seed(123)
> db1 <- fpc::dbscan(pred, eps = 1.5, MinPts = 5)
> fviz_cluster(db1, data = testing, stand = FALSE,
+              ellipse = FALSE, show.clust.cent = FALSE,
+              geom = "point",palette = "jco", ggtheme = theme_classic())

# Using DBScan on two attributes and Plotting for 2 attributes namely btc_total_bitcoins and btc_market_cap based on eps value of 0.15 and K= 5.

>bit <- bit_dataset_cleaned_filtered[,1:2]
>bit <- as.matrix(bit)
>dbscan::kNNdistplot(bit, k =  5)
 abline(h = 0.15, lty = 2)
> set.seed(123)
> db <- fpc::dbscan(bit, eps = 0.15, MinPts = 5)
> fviz_cluster(db, data = bit, stand = FALSE,
+                  ellipse = FALSE, show.clust.cent = FALSE,
+                  geom = "point",palette = "jco", ggtheme = theme_classic())
> print(db)

# Output obtained..
dbscan Pts=2920 MinPts=5 eps=0.15
       0    1 2  3
border 2    0 2  2
seed   0 2860 4 50
total  2 2860 6 52

# Using DBScan on two attributes and Plotting for 2 attributes namely btc_trade_volume and btc_market_cap based on eps value of 0.15 and K= 5.

bits <- bit_dataset_cleaned_filtered[c("btc_market_cap", "btc_trade_volume")]
bit <- as.matrix(bits)
set.seed(123)
>db <- fpc::dbscan(bit, eps = 0.15, MinPts = 5)
>fviz_cluster(db, data = bits, stand = FALSE,
                              ellipse = FALSE, show.clust.cent = FALSE,
                               geom = "point",palette = "jco", ggtheme = theme_classic())
							   
			
# Using DBScan on two attributes and Plotting for 2 attributes namely btc_trade_volume and btc_total_bitcoins based on eps value of 0.15 and K= 5.
bits <- bit_dataset_cleaned_filtered[c("btc_total_bitcoins", "btc_trade_volume")]
> bit2 <- as.matrix(bits)
>set.seed(123)
>db <- fpc::dbscan(bit, eps = 0.15, MinPts = 5)
>fviz_cluster(db, data = bits, stand = FALSE,
                              ellipse = FALSE, show.clust.cent = FALSE,
                               geom = "point",palette = "jco", ggtheme = theme_classic())
			


                       # CLARANS CLUSTERING - Partitioning Around Mediods (PAM Clustering)


> set.seed(1)
> library(cluster)
> library(lattice)
> library(latticeExtra)
> results <- clara(bit_dataset_cleaned_filtered, 4, metric = "euclidean", samples = 10, pamLike = FALSE)
> newbitset <- bit_dataset_cleaned_filtered
> newbitset$cluster <- results$clustering

# CLARANS CLUSTERING  based on 2 attributes namely btc_market_price_label and btc_market_cap

> A <- xyplot(btc_market_price_label ~ btc_market_cap, group = cluster, data = newbitset, auto.key = 
list(space = "right"), par.settings = list(superpose.symbol = list(pch = 0, cex = 2, col = c("green", "black", "red", "blue"))))
> C <- xyplot(results$medoids[,c("btc_market_price_label")] ~ results$medoids[,c("btc_market_cap")], pch = "@", cex = 1, col = c("green", "black", "red", "blue"), auto.key = list(space = "right"))
> D <- A + as.layer(C)
> plot(D) # Plotting the Clusters



> library(readr)
> bit_cash_price_cleaned <- read_csv("C:/Users/hits/Downloads/DA_Cryptocurrency-master/milestone2/Cleaned Dataset/bit_cash_price_cleaned.csv")
> View(bit_cash_price_cleaned)
> results <- clara(bit_cash_price_cleaned, 4, metric = "euclidean", samples = 10, pamLike = FALSE)
> newbitset <- bit_cash_price_cleaned
> newbitset$cluster <- results$clustering



 # CLARANS CLUSTERING  based on 2 attributes namely Volume and btc_market_cap
A <- xyplot(Volume ~ Market.Cap, group = cluster, data = newbitset, auto.key = list(space = "right"), par.settings = list(superpose.symbol = list(pch = 0, cex = 2, col = c("green", "black", "red", "blue"))))
> C <- xyplot(results$medoids[,c("Volume")] ~ results$medoids[,c("Market.Cap")], pch = "@", cex = 1, col = c("green", "black", "red", "blue"), auto.key = list(space = "right"))
> D <- A + as.layer(C)
> plot(D) # Plotting the Clusters
	                 
					 
					 

                       # CHAMELEON Based on Hierarchial Clustering
						
>library(seriation)
>data <- bit_dataset_cleaned_filtered
> data(Chameleon)
> plot(chameleon_ds4, cex=.1)
				
> library(readr)
> eth_dataset_cleaned <- read_csv("C:/Users/hits/Downloads/DA_Cryptocurrency-master/milestone2/Cleaned Dataset/eth_dataset_cleaned.csv")
> View(eth_dataset_cleaned)

> data <- eth_dataset_cleaned
> data(Chameleon)
> plot(chameleon_ds7, cex=.1)



                                 #DendroGramoGram for Hierarchial Clustering on Bitcoin Dataset
	
>library(amap)
>hc = hcluster(bit_dataset_cleaned_filtered)
>plot(hc, hang = -1, cex = 0.6)
> hcd = as.dendrogram(hc)
> plot(hcd)
> op = par(mfrow = c(2, 1))
> plot(cut(hcd, h = 75)$upper, main = "Upper tree of cut at h=75")
> plot(cut(hcd, h = 75)$lower[[2]], main = "Second branch of lower tree with cut at h=75")
>bit_dataset<- as.matrix(bit_dataset_cleaned_filtered[,1:8])  # Using first 8 Attributes in the Bitcoin Dataset for Plotting DendroGram
>bit_dataset
>hc = hcluster(bit_dataset)
plot(hc, hang = -1)
> hcd = as.dendrogram(hc)
> plot(hcd)
> plot(hc, hang = -1)


> bit_dataset<- as.matrix(bit_dataset_cleaned_filtered[,1:2])  # Using first 2 Attributes in the Bitcoin Dataset for Plotting DendroGram
> hc = hcluster(bit_dataset)
> hcd = as.dendrogram(hc)
> plot(hcd)
> plot(hcd,labels = names(bit_dataset))
> plot(hcd, type = "rectangle", ylab = "Height")
