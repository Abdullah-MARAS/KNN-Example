#knn  example adapted from Elif Kartal

###########################################################################
###########################################################################
#Credit risk analysis with KNN algorithm                     #
###########################################################################
###########################################################################


# Dataset
# https://archive.ics.uci.edu/ml/index.php
# (( wine )) https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.names

# Reading data as a dataframe
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data"

wine <- as.data.frame(read.table(file = url, header = FALSE, dec = ".", sep = ","))

# Changing column names
colnames(wine) <- c(
  "class",
  "Alcohol",
  "Malicacid",
  "Ash",
  "Alcalinityofash",
  "Magnesium",
  "Totalphenols",
  "Flavanoids",
  "Nonflavanoidphenols",
  "Proanthocyanins",
  "Colorintensity",
  "Hue",
  "OD280/OD315ofdilutedwines",
  "Proline")

# Reduce target variable number
table(wine$class)
#Changing typeof target variable
wine$class <- as.factor(wine$class)
levels(wine$class)
#Assign new class labels
levels(wine$class) <- c("1","2","2")
levels(wine$class)
table(wine$class)

# Changing the order of target variable
wine <- cbind(wine, wine$class)
wine <- wine[,-1]
colnames(wine)[14] <- "wineClass"

# Summary statistics of dataset
summary(wine)

# Target variables frequency counts
table(wine$wineClass)

# Normalization
install.packages("clusterSim")
library(clusterSim)
wine[,-14] <-data.Normalization(wine[,-14],type="n4",normalization="column")

summary(wine)

#Applying Knn Algorithm with Euclidean distance metric

# Data Partitioning
# install.packages("caret")
library(caret)
set.seed(1)
trainindices <- createDataPartition(y = wine$wineClass, p = .80, list = FALSE) 
trainindices[1:10]
trainset <- wine[trainindices,]
testset <- wine[-trainindices,]

table(wine$wineClass)
table(trainset$wineClass)
table(testset$wineClass)

# Seperating prective and target variables
testPredictive <- testset[, -14]
testTarget <- testset[[14]]

trainPredictive <- trainset[, -14]
trainTarget <- trainset[[14]]

# Assigning value of k for knn algorithm
k_value <- 10

install.packages("class")
library(class)
set.seed(1)
#To see the result in console use '()'
(Predictions <- knn(trainPredictive, testPredictive, trainTarget, k = k_value))

# Performance Evaluation
(resultTable <- table(Predictions, testTarget, dnn = c("Prediction", "Real Classes")))

(tp <- resultTable[1])
(fp <-  resultTable[3])
(fn <-  resultTable[2])
(tn <-  resultTable[4])

# Plotting the confusion matrix
fourfoldplot( resultTable, color = rainbow(2))

