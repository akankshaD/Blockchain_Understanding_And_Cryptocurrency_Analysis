
#PCA
library(timeSeries)
# Reading the file
> bit_price_cleaned <- read_csv("Path")
# Applying the timeseries function
> bit_price_cleaned_ts <- ts(bit_price_cleaned)
> p<-ncol(bit_price_cleaned_ts)
#placing the eigenvalues in L
> L<-bitegv$values
> Vm<-matrix(0,nrow=p,ncol=p)
#putting the eigenvalues in the diagonals
> diag(Vm)<-L
> Vm 
#these are the eigenvectors-- these are the standardized regression weights
> bitegv$vectors
[,1]        [,2]        [,3]       [,4]        [,5]        [,6]         [,7]
[1,] -0.2688881  0.95952192 -0.07236634 -0.0309644  0.02353564 -0.01597592 -0.003471412
[2,] -0.3972197 -0.10729337  0.19272135  0.3239993  0.47308633 -0.45157205 -0.510744111
[3,] -0.3974444 -0.11498021  0.15147049 -0.2170550  0.60369596  0.28602017  0.559021418
[4,] -0.3970290 -0.09519812  0.23463914 -0.1533898 -0.48266102 -0.59948161  0.402965885
[5,] -0.3972028 -0.10673412  0.18473173 -0.5959776 -0.23780932  0.35353971 -0.509881418
[6,] -0.3711879 -0.17376976 -0.90954819 -0.0241632 -0.02553496 -0.05904972 -0.004788588
[7,] -0.3984609 -0.06272634  0.13497792  0.6838503 -0.34787875  0.47556758  0.065114977
#these are the loadings
#or the correlation of the component variables with the original variables-- sometimes referred to as the P matrix. And PP` is the original correlation matrix.
> loadings<-bitegv$vectors %*% sqrt(Vm)
#We can use the eigenvalues and the eigenvectors.
> bitegv$vectors %*% Vm %*% t(bitegv$vectors)
#To reproduce the original correlation matrix (just shown again below):
> cor(bit_price_cleaned_ts)
Date      Open      High       Low     Close    Volume Market.Cap
Date       1.0000000 0.6048643 0.6013488 0.6110190 0.6053073 0.5347978  0.6328919
Open       0.6048643 1.0000000 0.9990631 0.9987178 0.9979989 0.9078130  0.9981978
High       0.6013488 0.9990631 1.0000000 0.9983421 0.9990468 0.9145967  0.9971075
Low        0.6110190 0.9987178 0.9983421 1.0000000 0.9990506 0.9006568  0.9973879
Close      0.6053073 0.9979989 0.9990468 0.9990506 1.0000000 0.9088467  0.9963552
Volume     0.5347978 0.9078130 0.9145967 0.9006568 0.9088467 1.0000000  0.9136942
Market.Cap 0.6328919 0.9981978 0.9971075 0.9973879 0.9963552 0.9136942  1.0000000
> L/length(L)
[1] 0.8935581647 0.0848749013 0.0207653320 0.0004949140 0.0001945313 0.0000820746 0.0000300821

#To compute the PCA scores (using matrices), zValues x eigenvectors:
> zdat <- scale(bit_price_cleaned_ts)
> pca.scores <- zdat %*% bitegv$vectors
> colnames(pca.scores)<-c('pca1','pca2','pca3','pca4','pca5','pca6', 'pca7')
> head(pca.scores)
> apply(pca.scores, 2, var)
pca1         pca2         pca3         pca4         pca5         pca6         pca7 
6.2549071529 0.5941243091 0.1453573240 0.0034643978 0.0013617193 0.0005745222 0.0002105747 
#variance computed above and eigen values are same
> bitegv$values
[1] 6.2549071529 0.5941243091 0.1453573240 0.0034643978 0.0013617193 0.0005745222 0.0002105747
#You can also use the eigenvalues to draw a scree plot 
> plot(L,main="Scree Plot",ylab="Eigenvalues",xlab="Component number",type='b')
> L
[1] 6.2549071529 0.5941243091 0.1453573240 0.0034643978 0.0013617193 0.0005745222 0.0002105747
> hist(L)
> cov(bit_price_cleaned_ts)
Date        Open        High         Low       Close      Volume  Market.Cap
Date       218835.0000 282.9543486 281.3097800 285.8335089 283.1615422 250.1773673 296.0656006
Open          282.9543   1.0000000   0.9990631   0.9987178   0.9979989   0.9078130   0.9981978
High          281.3098   0.9990631   1.0000000   0.9983421   0.9990468   0.9145967   0.9971075
Low           285.8335   0.9987178   0.9983421   1.0000000   0.9990506   0.9006568   0.9973879
Close         283.1615   0.9979989   0.9990468   0.9990506   1.0000000   0.9088467   0.9963552
Volume        250.1774   0.9078130   0.9145967   0.9006568   0.9088467   1.0000000   0.9136942
Market.Cap    296.0656   0.9981978   0.9971075   0.9973879   0.9963552   0.9136942   1.0000000



#PCA using prcomp function

> prcomp(bit_price_cleaned_ts)
Standard deviations (1, .., p=7):
  [1] 467.80033542   1.92213934   0.38202797   0.05888725   0.03691168   0.02397225   0.01451128

Rotation (n x k) = (7 x 7):
  PC1          PC2          PC3           PC4           PC5           PC6           PC7
Date       -0.999995079 -0.003133685 -0.000119188  6.593691e-05 -5.020754e-05  3.410468e-05  7.415991e-06
Open       -0.001293019  0.412275547  0.190707465 -3.232354e-01 -4.737197e-01  4.515786e-01  5.107281e-01
High       -0.001285504  0.414362866  0.149002224  2.176734e-01 -6.035283e-01 -2.862900e-01 -5.590260e-01
Low        -0.001306175  0.409005434  0.233361137  1.532213e-01  4.825080e-01  5.996422e-01 -4.029812e-01
Close      -0.001293965  0.412046586  0.182751318  5.958523e-01  2.383280e-01 -3.535312e-01  5.098954e-01
Volume     -0.001143239  0.400329357 -0.913777380  2.415844e-02  2.552074e-02  5.906054e-02  4.786799e-03
Market.Cap -0.001352932  0.401236888  0.136324691 -6.848626e-01  3.479622e-01 -4.754698e-01 -6.509006e-02
> par(mfrow=c(2,1))
> unscaled <- prcomp(bit_price_cleaned_ts)
> par(mfrow=c(2,1))
> plot(unscaled)
> scaled <- prcomp(bit_price_cleaned_ts, scale = TRUE)
> plot(scaled)
> summary(scaled)
Importance of components%s:
  PC1     PC2     PC3     PC4     PC5     PC6     PC7
Standard deviation     2.5010 0.77079 0.38126 0.05886 0.03690 0.02397 0.01451
Proportion of Variance 0.8936 0.08487 0.02077 0.00049 0.00019 0.00008 0.00003
Cumulative Proportion  0.8936 0.97843 0.99920 0.99969 0.99989 0.99997 1.00000
> summary(unscaled)
Importance of components%s:
  PC1     PC2   PC3     PC4     PC5     PC6     PC7
Standard deviation     467.8 1.92214 0.382 0.05889 0.03691 0.02397 0.01451
Proportion of Variance   1.0 0.00002 0.000 0.00000 0.00000 0.00000 0.00000
Cumulative Proportion    1.0 1.00000 1.000 1.00000 1.00000 1.00000 1.00000
> biplot(unscaled)
> unscaled

#calculate covariance and eigen
> bitcov <- cov(bit_price_cleaned_ts, bit_price_cleaned_ts, method = c("pearson", "kendall", "spearman"))
> bitcor <- cor(bit_price_cleaned_ts, bit_price_cleaned_ts, method = c("pearson", "kendall", "spearman"))
> bitegv <- eigen(bitcov)