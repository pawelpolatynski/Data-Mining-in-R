# Paweł Połatyński EDAMI Lab 4 Classification
library(gmodels) #results analysis
library(Hmisc) #results analysis
library(caret)
library(rpart) # rpart() - decision tree classifier
library(rpart.plot) 
library(e1071)
library(C50) # C5 classifer
library(randomForest)


download.file('http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv', 'wine_red.csv');
wineRed_ds = read.table("wine_red.csv", header = TRUE, sep=";", na.strings= "*")

#creating a new attribute with 3 values(classes) based on 
# the orignal class atribute - quality
wineRed_ds$Q2 = lapply(wineRed_ds[,12], function (x)
{
  if(x >6)  { "A"}
  else if(x >4)  {"B"}
  else { "C"}   
})

wineRed_ds$Q2 = unlist(wineRed_ds$Q2);

wineRed_ds$Q2 = as.factor(wineRed_ds$Q2)

# I decided to remove atribute "quallity"
wineRed_ds <- wineRed_ds[, -12]

################# Aim of the experiment ###################

# The aim of the experiment is to find the best possible classifier for this dataset.
# The calss atribute in this data set is newly created Q2 atribute. It contains 3 classes: A, B and C.
# To evaluate quality of classification I will use the average 
# precision (number of items correctly classified to the class divided by number of all elements in the class) of every class.

# The classification already existing in this dataset can help customers choose wine worth buying. It can be also used
# to adjust price of wine to it's quality.

##################### Experiments #########################

idTrainData <- unlist(createDataPartition(wineRed_ds$Q2,p=0.7))
#str(idTrainData)

wineTrain <-wineRed_ds[idTrainData,]
wineTest <-wineRed_ds[-idTrainData,]

prop.table(table(wineTrain$Q2))
prop.table(table(wineTest$Q2))


wine_C50 <- C5.0(wineTrain[,-12], wineTrain$Q2) 
plot(wine_C50)

########## First try - C5 classificator withour rules

wine_C50_trainPred <- predict(wine_C50, wineTrain)

wine_C50_testPred <- predict(wine_C50, wineTest)
CrossTable(wine_C50_testPred, wineTest$Q2, prop.chisq = FALSE,prop.c = FALSE, 
           prop.r = FALSE, dnn = c('predicted class', 'actual class'))

"                | actual class 
predicted class |         A |         B |         C | Row Total | 
----------------|-----------|-----------|-----------|-----------|
              A |        25 |        15 |         0 |        40 | 
                |     0.052 |     0.031 |     0.000 |           | 
----------------|-----------|-----------|-----------|-----------|
              B |        38 |       372 |        15 |       425 | 
                |     0.079 |     0.778 |     0.031 |           | 
----------------|-----------|-----------|-----------|-----------|
              C |         2 |         8 |         3 |        13 | 
                |     0.004 |     0.017 |     0.006 |           | 
----------------|-----------|-----------|-----------|-----------|
   Column Total |        65 |       395 |        18 |       478 | 
----------------|-----------|-----------|-----------|-----------|"

confusionMatrix(wine_C50_testPred, wineTest$Q2, mode="everything")

"          Reference
Prediction   A   B   C
         A  25  15   0
         B  38 372  15
         C   2   8   3

Overall Statistics
                                          
               Accuracy : 0.8368          
                 95% CI : (0.8006, 0.8688)
    No Information Rate : 0.8264          
    P-Value [Acc > NIR] : 0.296675        
                                          
                  Kappa : 0.3547          
                                          
 Mcnemar's Test P-Value : 0.002757 
 
 Statistics by Class:
                      Class: A Class: B Class: C
Precision             0.62500   0.8753 0.230769
Recall                0.38462   0.9418 0.166667
"

####### Second try - C5 classificator with rules

wine_C50R <- C5.0(wineTrain[,-12], wineTrain$Q2,  rules = TRUE) 

wine_C50_testPred <- predict(wine_C50R, wineTest)
CrossTable(wine_C50_testPred, wineTest$Q2, prop.chisq = FALSE,prop.c = FALSE, 
           prop.r = FALSE, dnn = c('predicted class', 'actual class'))


"                | actual class 
predicted class |         A |         B |         C | Row Total | 
----------------|-----------|-----------|-----------|-----------|
              A |        25 |        15 |         0 |        40 | 
                |     0.052 |     0.031 |     0.000 |           | 
----------------|-----------|-----------|-----------|-----------|
              B |        40 |       376 |        16 |       432 | 
                |     0.084 |     0.787 |     0.033 |           | 
----------------|-----------|-----------|-----------|-----------|
              C |         0 |         4 |         2 |         6 | 
                |     0.000 |     0.008 |     0.004 |           | 
----------------|-----------|-----------|-----------|-----------|
   Column Total |        65 |       395 |        18 |       478 | 
----------------|-----------|-----------|-----------|-----------|
"
confusionMatrix(wine_C50_testPred, wineTest$Q2, mode="everything")

"          Reference
Prediction   A   B   C
         A  25  15   0
         B  40 376  16
         C   0   4   2

Overall Statistics
                                          
               Accuracy : 0.8431          
                 95% CI : (0.8073, 0.8745)
    No Information Rate : 0.8264          
    P-Value [Acc > NIR] : 0.1832          
                                          
                  Kappa : 0.3498          
                                          
 Mcnemar's Test P-Value : NA              

Statistics by Class:

                     Class: A Class: B Class: C
Precision             0.62500   0.8704 0.333333
Recall                0.38462   0.9519 0.111111
"
###### 3rd try - Recursive partitioning trees

wineFormula <-  Q2 ~ fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol

wine_rpart <- rpart(wineFormula,  method="class", data=wineTrain)
print(wine_rpart)

wine_rpat_testPred = predict(wine_rpart,wineTest,type = "class")
table(wine_rpat_testPred, wineTest$Q2)

confusionMatrix(wine_rpat_testPred, wineTest$Q2, mode="everything")

"          Reference
Prediction   A   B   C
         A  29  10   0
         B  36 380  16
         C   0   5   2

Overall Statistics
                                          
               Accuracy : 0.8598          
                 95% CI : (0.8254, 0.8897)
    No Information Rate : 0.8264          
    P-Value [Acc > NIR] : 0.02819         
                                          
                  Kappa : 0.4196          
                                          
 Mcnemar's Test P-Value : NA              

Statistics by Class:
                     Class: A Class: B Class: C
Precision             0.74359   0.8796 0.285714
Recall                0.44615   0.9620 0.111111

"

# 4th try - random forest without tunning parameters

wine_Forest = randomForest(Q2~., data = wineTrain, importance = TRUE, nodesize = 10, mtry = 4, ntree = 100 )
wine_Forest_testPred = predict (wine_Forest, newdata = wineTest[-12])
confusionMatrix(wine_Forest_testPred, wineTest$Q2, mode = "everything")

"         Reference
Prediction   A   B   C
         A  27  12   0
         B  38 383  17
         C   0   0   1

Overall Statistics
                                          
               Accuracy : 0.8598          
                 95% CI : (0.8254, 0.8897)
    No Information Rate : 0.8264          
    P-Value [Acc > NIR] : 0.02819         
                                          
                  Kappa : 0.3948          
                                          
 Mcnemar's Test P-Value : NA              

Statistics by Class:

                     Class: A Class: B Class: C
Precision             0.69231   0.8744 1.000000
Recall                0.41538   0.9696 0.055556
"

# 5th try - random forest after tunning parameters

trControl <- trainControl(method = "cv", number = 10, search = "grid")

tuneGrid <- expand.grid(mtry = c(1:6))
tuneGrid
wine_Frestores_mtry <- train(Q2~.,  data = wineTrain,
                            method = "rf",
                            metric = "Accuracy",
                            tuneGrid = tuneGrid,
                            trControl = trControl,
                            importance = TRUE,    # randomForest function parameter
                            nodesize = 10,        # randomForest function parameter
                            ntree = 250)          ## randomForest function parameter


treesModels <- list()
for (nbTree in c(5,10,25, 50, 100, 250, 500)) 
{
  wine_F_maxtrees <- train(Q2~.,  data = wineTrain,
                          method = "rf",
                          metric = "Accuracy",
                          tuneGrid = tuneGrid,
                          trControl = trControl,
                          importance = TRUE,
                          nodesize = 10,
                          ntree = nbTree)
  key <- toString(nbTree)
  treesModels[[key]] <- wine_F_maxtrees
}

results_tree <- resamples(treesModels)
summary(results_tree)


wine_Forest2 = randomForest(Q2~., data = wineTrain, importance = TRUE, mtry = 6, ntree = 25, nodesize = 10)

print(car_Forest2)
plot(car_Forest2)


wine_Forest2_testPred = predict (wine_Forest2, newdata = wineTest[-12])
confusionMatrix(wine_Forest2_testPred, wineTest$Q2, mode = "everything")

"          Reference
Prediction   A   B   C
         A  32  13   0
         B  33 382  16
         C   0   0   2

Overall Statistics
                                          
               Accuracy : 0.8703          
                 95% CI : (0.8368, 0.8991)
    No Information Rate : 0.8264          
    P-Value [Acc > NIR] : 0.00535         
                                          
                  Kappa : 0.4639          
                                          
 Mcnemar's Test P-Value : NA              

Statistics by Class:

                     Class: A Class: B Class: C
Precision             0.71111   0.8863 1.000000
Recall                0.49231   0.9671 0.111111
"

###################### Conclsuion #########################
"We can present acquired outcomes' precisions in a table:

                     Class: A Class: B Class: C
C5 wo Rules           0.62500   0.8753 0.230769
C5 with Rules         0.62500   0.8704 0.333333
RPT                   0.74359   0.8796 0.285714
Random Forest         0.69231   0.8744 1.000000
Tuned RF              0.71111   0.8863 1.000000

The most items where included in class B so I will use this precision as the most important one.
The last 2 tries had 100% precision in class C but they assigned to class C only 1 element (2 elements in Tuned RF), 
so it won't be a factor in choosing the best classifier.
Every classifier got very similar preciosion in assigning items to class B.
In class A C5 classifier got much worse precision than the rest of the classifiers.
Tuning Random Forest classificator takes some time and (for this dataset) doesn't return better classification than recursive partitioning trees.
In my opinion the best classifier for this dataset is RPT because of it's simplicity and precision.
"