# Paweł Połatyński EDAMI lab 1 : Association Rules
# 27.04.2020

#
##
### Aim of experiment
##
#

# The rules I am intereseted in, are rules with lift value grater than and with support greater than 0.2. .
# The higher value of lift the better.
# This rules could be use to better target advertisements or suggest other items that could be added to
# a shopping cart.

#
##
### Experiments
##
# 

# Downloading of the data set

download.file('http://staff.ii.pw.edu.pl/~gprotazi/dydaktyka/dane/supermarket.csv','supermarket.csv')
marketSet = read.csv('supermarket.csv',sep=';')
marketSet= as.data.frame(sapply(marketSet, function(x) as.logical(x)))

library(arules) # association rules
library(arulesViz) # visualization of reles

# Deleting of columns containing only one value

delOneValued <- function(inputData11)
{
  res <- c(which(sapply(inputData11, function(x) {length(unique(x))}) == 1));
  if(length(res) > 0)         
  {
    data11 <- inputData11[,-res];
  }   
  else
  {
    data11 <-inputData11;
  }
  data11 
}

#testing

marketSet2 <- delOneValued(marketSet)

str(marketSet2)
dim(marketSet2)
View(marketSet2)

# Transforming dataset into transactional form

marketTR <- as(marketSet2, "transactions")

# Finding most frequent itemsets

freqTbl  = itemFrequency(marketTR, type = "relative")
freqTbl = sort(freqTbl, decreasing= TRUE)
View(freqTbl)


#
# First I am looking at number of rules dependence of value of support.
#

# Using apriori algorythm to find all association rules with confidence greater or equal to 0.4,
# support 0.4, and length 2
 

aParam  = new("APparameter", "confidence" = 0.4, "support" = 0.4, "minlen"= 2) 
mSshort412 = apriori(marketTR, aParam)

inspect(mSshort412)
# There are 30 rules. Each rule has lift value greater than 1.

# Using apriori algorythm to find all association rules with confidence greater or equal to 0.4,
# support 0.1, and length 2


aParam  = new("APparameter", "confidence" = 0.4, "support" = 0.5, "minlen"= 2) 
mSshort452 = apriori(marketTR, aParam)

inspect(mSshort452)
# There are 4 rules. Each rule has lift value greater than 1.

# Using apriori algorythm to find all association rules with confidence greater or equal to 0.4,
# support 0.6, and length 2


aParam  = new("APparameter", "confidence" = 0.4, "support" = 0.3, "minlen"= 2) 
mSshort432 = apriori(marketTR, aParam)

inspect(mSshort432)
# There are 184 rules.

#
# I am looking at the lift values of discovered rules.
#

sortedLift = sort(mSshort432, by = "lift", decreasing= FALSE)

# As you can see, all discovered rules have lift values greater than 1. The greatest value of lift in this set of rules
# is 1.37. Either all rules happen to have lift greater than 1 or the algorithm picks the ones that have.

#
# Now I am comapring average confidence of "long" and "short" rules.
#

aParam  = new("APparameter", "confidence" = 0.7, "support" = 0.2, "minlen"= 2) 
mSshort722 = apriori(marketTR, aParam)

aParam  = new("APparameter", "confidence" = 0.7, "support" = 0.2, "minlen"= 4) 
mSlongt724 = apriori(marketTR, aParam)

aParam  = new("APparameter", "confidence" = 0.7, "support" = 0.2, "minlen"= 5) 
mSlongt725 = apriori(marketTR, aParam)

summary(mSshort722)

# summary of quality measures:
#  support         confidence          lift           count     
# Min.   :0.2001   Min.   :0.7000   Min.   :1.052   Min.   : 926  
# 1st Qu.:0.2075   1st Qu.:0.7216   1st Qu.:1.136   1st Qu.: 960  
# Median :0.2301   Median :0.7444   Median :1.168   Median :1064  
# Mean   :0.2428   Mean   :0.7622   Mean   :1.179   Mean   :1123  
# 3rd Qu.:0.2610   3rd Qu.:0.8050   3rd Qu.:1.215   3rd Qu.:1208  
# Max.   :0.5051   Max.   :0.8941   Max.   :1.377   Max.   :2337  

summary(mSlongt724)

# summary of quality measures:
#   support         confidence          lift           count     
# Min.   :0.2006   Min.   :0.7001   Min.   :1.115   Min.   : 928  
# 1st Qu.:0.2040   1st Qu.:0.7311   1st Qu.:1.167   1st Qu.: 944  
# Median :0.2099   Median :0.7540   Median :1.192   Median : 971  
# Mean   :0.2198   Mean   :0.7749   Mean   :1.206   Mean   :1017  
# 3rd Qu.:0.2344   3rd Qu.:0.8280   3rd Qu.:1.244   3rd Qu.:1084  
# Max.   :0.2833   Max.   :0.8941   Max.   :1.377   Max.   :1311  

summary(mSlongt725)

# summary of quality measures:
#   support         confidence          lift           count    
# Min.   :0.2008   Min.   :0.7162   Min.   :1.178   Min.   :929  
# 1st Qu.:0.2008   1st Qu.:0.7522   1st Qu.:1.203   1st Qu.:929  
# Median :0.2019   Median :0.8060   Median :1.259   Median :934  
# Mean   :0.2019   Mean   :0.7981   Mean   :1.251   Mean   :934  
# 3rd Qu.:0.2029   3rd Qu.:0.8253   3rd Qu.:1.289   3rd Qu.:939  
# Max.   :0.2029   Max.   :0.8941   Max.   :1.329   Max.   :939  


# rules set length     lift
# 2                     1.179
# 4                     1.206
# 5                     1,251

# Looking at this three sets of rule we can come to a conclusion, that the "longer" are the rules, the greater is
# the average value of their lift.

plot(mSshort722, jitter = 0)

#
##
### Conclusion
##
#

# Looking at this plot we can come to a conclusion that rules with lower confidence and support, usually have greater
# value of lift.

# The longer the itemset the harder it is to find association rules containing it and the lower are values of its
# support. It is easier to find association rules with short itemsets.

# The most common consequent rule is the most frequent item.

