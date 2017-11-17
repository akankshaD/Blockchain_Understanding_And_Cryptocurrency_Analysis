> library(readr)
>library(timeSeries)
>library(hornpa)
# Reading the file
> eth_dataset_cleaned <- read_csv("C:/Users/hits/Downloads/DA_Cryptocurrency-master/milestone2/Cleaned Dataset/eth_dataset_cleaned.csv")
> View(eth_dataset_cleaned)
> col <- ncol(eth_dataset_cleaned)
# Applying the timeseries function
> dat <- ts(eth_dataset_cleaned[1:17])
> col <- ncol(dat)
> cor <- cor(dat) # correlation matrix
#solving for the eigenvalues and eigenvectors from the correlation matrix
> eigen <- eigen(cor)
#storing the eigenvalues in evalues
> evalues <- eigen$values
#Vm is an orthogonal matrix since all correlations between variable are 0.
> Vm<-matrix(0,nrow=col,ncol=col)
#putting the eigenvalues in the diagonals
> diag(Vm)<-evalues
> Vm
#storing the eigenvalues in evectors
> evectors <- eigen$vectors
# The correlation of the component variables with the original variables-- sometimes referred to as the P matrix. And PP` is the original correlation matrix.
> loadings<-evectors %*% sqrt(Vm)
# To reproduce the original correlation matrix
> cor(dat)
# using the eigenvalues and the eigenvectors.  
> evectors %*% Vm %*% t(evectors)
#this is just to standardize the original data, M = 0, SD =1    
> zdat<-scale(dat)
#scaled values x vectors
> pca.scores<- zdat %*% evectors
> colnames(pca.scores)<-c('pca1','pca2','pca3','pca4','pca5','pca6', 'pca7', 'pca8', 'pca9', 'pca10', 'pca11', 'pca12', 'pca13', 'pca14','pca15','pca16', 'pca17')
> head(pca.scores)
         pca1       pca2       pca3      pca4        pca5       pca6     pca7       pca8
[1,] 5.435919  2.8112830 4.33361039 3.4926634 -0.06675838  0.5310197 1.989059 -1.1361282
[2,] 4.265216  0.3838533 1.01619351 0.9975713 -0.82471598 -0.8917637 2.200989 -1.6803348
[3,] 3.100191 -2.1549181 0.04394677 0.9720727 -1.02761193 -1.8492387 1.334089 -0.6920902
[4,] 3.193514 -1.9552205 0.28013716 1.1818252 -0.93045737 -1.7408295 1.311342 -0.6747394
[5,] 3.162328 -1.9856604 0.47878157 1.3833176 -0.89615011 -1.7299257 1.162254 -0.5836881
[6,] 3.235097 -1.8234446 0.54787350 1.4047993 -0.86839810 -1.6678540 1.212154 -0.6300597
           pca9      pca10      pca11       pca12       pca13       pca14      pca15
[1,] -0.2879885 -1.4615159  0.7881429 -0.71799715  0.46361039  0.24455109 0.13787928
[2,] -0.7118600 -0.1651849 -0.4161614  0.24538214 -0.19121357 -0.09810336 0.01933213
[3,] -0.0938686 -0.2390994 -0.1387209  0.05754798 -0.04903718 -0.02073213 0.01810867
[4,] -0.1242868 -0.2511927 -0.1295707  0.06264516 -0.05044316 -0.01919615 0.01967946
[5,] -0.1125615 -0.2493749 -0.1082043  0.07193485 -0.04787284 -0.01483221 0.01945455
[6,] -0.1432738 -0.2544748 -0.1154568  0.07431069 -0.05111157 -0.01737194 0.02026897
             pca16        pca17         
[1,] -0.0200004885  0.006646296 
[2,]  0.0008072468 -0.005023711  
[3,] -0.0080728462  0.001706592  
[4,] -0.0028625665  0.001644196 
[5,]  0.0037480274  0.001917612 
[6,]  0.0026735856  0.001614332 
> comp.matrix<-evectors %*% sqrt(Vm)
> comp.matrix
#If you square the correlations, this will give you how much variance the PC accounts for in the original variables. These are referred to as the communalities.
> comp.matrix[,1]^2
 [1] 0.59634853 0.59634853 0.91740178 0.95531789 0.96500245 0.58504288 0.92158498
 [8] 0.97696382 0.92973252 0.46986368 0.07148638 0.93735990 0.54109097 0.02331028
[15] 0.55861560 0.94198507 0.46412450 0.29565653
#You can also use the eigenvalues to draw a SCREE PLOT 

> plot(evalues,main="Scree Plot",ylab="Eigenvalues",xlab="Component number",type='b')abline(h=1, lty=2)


#From the hornpa function: The program generates a specified number of random datasets based on the number of variables entered by the user. As the correlation matrices have an eigenvalue based on random noise, these eigenvalues are computed for the each dataset and collected. The mean and the specified percentile (95th is the default) are computed. The output table shows how large eigenvalues can be as a result of merely using randomly generated datasets. If the user’s own dataset has an actual eigenvalue greater than the generated eigenvalue (which is based on random noise), that lends support to retain that factor. In other words, if the i(th) eigenvalue from the actual data was larger than the percentile of the (i)th eigenvalue generated using randomly generated data, empirical support is provided to retain that factor. Install the hornpa package if you have not already done so.

#Retaining the components
 library(hornpa)
> hornpa(k=17,size=797,reps=300,seed=123)

 #Parallel Analysis Results  
 
Method: pca 
Number of variables: 17 
Sample size: 797 
Number of correlation matrices: 300 
Seed: 123 
Percentile: 0.95 
 
#Compare your observed eigenvalues from your original dataset to the 95 percentile in the table below generated using random data. If your eigenvalue is greater than the percentile indicated (not the mean), you have support to retain that factor/component. 
 
 Component  Mean  0.95
         1 1.258 1.303
         2 1.208 1.243
         3 1.167 1.196
         4 1.132 1.163
         5 1.102 1.126
         6 1.074 1.099
         7 1.047 1.067
         8 1.021 1.042
         9 0.994 1.015
        10 0.968 0.991
        11 0.944 0.963
        12 0.917 0.938
        13 0.892 0.913
        14 0.865 0.889
        15 0.837 0.863
        16 0.806 0.832
        17 0.766 0.796
> evalues
 [1]  1.174724e+01  2.798011e+00  1.050926e+00  8.386273e-01  7.489018e-01  4.517560e-01
 [7]  1.772037e-01  7.866466e-02  4.926496e-02  3.501539e-02  1.304877e-02  7.186283e-03
[13]  2.150097e-03  1.538856e-03  2.618421e-04  1.987927e-04  8.181825e-06 -1.441532e-15