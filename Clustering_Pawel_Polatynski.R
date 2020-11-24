# Clustering Paweł Połatyński 279117
#
# The goal of the laboratory task is to perform tests to find the best way (the most similar to the reference grouping)
# to group data provided in this dataset.
# Function calculating the accuracy of clustering


accuracyCalc <- function(confTbl, startCol)
{
  corr = 0;
  for(i in startCol:ncol(confTbl))
  {
    corr = corr + max(confTbl[,i])  
  }
  accuracy = corr/sum(confTbl)
  accuracy  
}

View(wineWhite_ds)
library(dbscan)
library(fpc)
library(cluster)
library(factoextra)
set.seed(7777)

# Downloading the data and saving it into dataset
download.file('http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv', 'wine_white.csv');

wineWhite_ds = read.table("wine_white.csv", header = TRUE, sep=";", na.strings= "*")
wineWhite_dsC <- wineWhite_ds[,-12]

# Starting value of accuracy 

# The final value of accuracy of the task should be greater than the one from example below:

card.kmeans = kmeans(wineWhite_dsC,6)
res3 = table(wineWhite_ds$quality,card.kmeans$cluster )
res3
accuracyCalc(res3,1)


# Function's outcome
# [1] 0.4577379
# We're looking for clustering parameters delivering clustering with greater accuracy

?kmeans


### 1st test
# I start with setting clusters numbmer to 7 which is the same number of groups as in the reference grouping.

card.kmeans = kmeans(wineWhite_dsC,7)
res3 = table(wineWhite_ds$quality,card.kmeans$cluster )
res3
accuracyCalc(res3,1)

# Outcome: [1] 0.4524296
# Comment: The outcome is worse than the one we refer to. It's surprising that (although the amount of clusters 
# we got is the same as number of clusters in reference clustering) the accuracy is lower.



### 2nd test
# I set clusters numbmer to 8.

card.kmeans = kmeans(wineWhite_dsC,8)
res3 = table(wineWhite_ds$quality,card.kmeans$cluster )
res3
accuracyCalc(res3,1)

# Outcome: [1] 0.454267
# Comment: The outcome is still slightly worse than the one at the begining.

### 3rd test
# I set clusters number to 10 (maximal numbers of clusters allowed by task)

card.kmeans = kmeans(wineWhite_dsC,10)
res3 = table(wineWhite_ds$quality,card.kmeans$cluster )
res3
accuracyCalc(res3,1)

# Outcome: [1] 0.456717
# Comment: The outcome is still worse than the one at the begining but has higher accuracy than test where centers parameter was 8.

### 4th test
# I set clusters number to 4.

card.kmeans = kmeans(wineWhite_dsC,4)
res3 = table(wineWhite_ds$quality,card.kmeans$cluster )
res3
accuracyCalc(res3,1)

# Outcome: [1] 0.4510004
# Comment: The outcome is much worse than the one at the begining.

### Conclusion 1: The more cluster the easier it is for algorithm to cluster data corectly. 

### 5th test
# I set clusters number to 6 and iter.max = 100.

card.kmeans = kmeans(wineWhite_dsC,centers = 6, iter.max = 100)
res3 = table(wineWhite_ds$quality,card.kmeans$cluster )

accuracyCalc(res3,1)

# Outcome: [1] 0.457942
# Comment: The outcome is better than the one at the begining. Increaing number of iteration can increase the accuracy.

### 6th test
# I set clusters number to 6 and nstart to 100.

card.kmeans = kmeans(wineWhite_dsC,centers = 6, nstart = 100)
res3 = table(wineWhite_ds$quality,card.kmeans$cluster )

accuracyCalc(res3,1)

# Outcome: [1] [1] 0.4577379
# Comment: The outcome is very similar to the one at the begining.


### 7th test
# I set clusters number to 6 and nstart 1.

card.kmeans = kmeans(wineWhite_dsC,centers = 6, nstart = 1)
res3 = table(wineWhite_ds$quality,card.kmeans$cluster )

accuracyCalc(res3,1)

# Outcome: [1] 0.4583503
# Comment: Number of random sets doesn't seem to make mouch of a difference for accuracy.

### 8th test
# I set clusters number to 6 and using Hartigan-Wong algorithm.

card.kmeans = kmeans(wineWhite_dsC,centers = 6, algorithm = "Hartigan-Wong")
res3 = table(wineWhite_ds$quality,card.kmeans$cluster )

accuracyCalc(res3,1)

# Outcome: [1] 0.4577379

### 9th test
# I set clusters number to 6 and using Lloyd algorithm.

card.kmeans = kmeans(wineWhite_dsC,centers = 6, iter.max = 50, algorithm = "Lloyd")
res3 = table(wineWhite_ds$quality,card.kmeans$cluster )

accuracyCalc(res3,1)

# Outcome: [1] 0.4577379

### 10th test
# I set clusters number to 6 and using Forgy algorithm.

card.kmeans = kmeans(wineWhite_dsC,centers = 6,iter.max = 50, algorithm = "Forgy")
res3 = table(wineWhite_ds$quality,card.kmeans$cluster )

accuracyCalc(res3,1)

# Outcome: [1] 0.4550837


# Now I will try to scale the data and see if it will improce the accuracy.

wineScale <- scale(wineWhite_dsC, center = TRUE, scale = TRUE)

### 11th test
# I set clusters numbmer to 7, the same as in test 1.

card.kmeans = kmeans(wineScale,10, iter.max = 100)
res3 = table(wineWhite_ds$quality,card.kmeans$cluster )
res3
accuracyCalc(res3,1)

# Outcome: [1] 0.4767252
# Comment: Scaling and increasing number of iterations slightly increased the accuracy of clustering (by 0.02).

#####################################################
# Conclusion: Durning this task I managed to improve accuracy of clustering just a bit. Changing the value of paramter "nstart"
#             didn't improve nor diminished the accuracy. Increasing the iter.max value helped with accuracy but only to some point.
#             Using different algorithms usually led to the same output. Some of them only needed more iteration steps to suceed.
#             What helped the most with increasing the accuracy was increasing number of clusters and scaling the data. I think that
#             that the problem with improving clustering here is caused by the dataset. I don't know much about wine but I don't think
#             that quality of the wine can be easily designated just by looking at the parameters.

