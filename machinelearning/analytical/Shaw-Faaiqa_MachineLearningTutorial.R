
# =========================== 00. INTRODUCTION ==================================================

# This tutorial has been developed by F. Atiyya Shaw. 
# Please direct questions to atiyya@gatech.edu.
# 11/25/2020

# Sources: 
# https://machinelearningmastery.com/machine-learning-in-r-step-by-step/
# https://en.wikipedia.org/wiki/Iris_flower_data_set
# https://www.datacamp.com/community/tutorials/machine-learning-in-r
# https://lgatto.github.io/IntroMachineLearningWithR/an-introduction-to-machine-learning-with-r.html

# 01. Define problem that we are trying to solve 
# 02. Discuss and prepare coding environment
# 03. Import/load + prepare data for use in coding environment
# 04. Explore + visualize + pre-process data
# 05. Run and evaluate algorithms
# 06. What's next?

# =========================== 01. PROBLEM ===================================================

# We have a dataset that contains 150 observations of iris flowers. This dataset contains 
# four columns of measurements of the flowers (in cm). The fifth column contains the species
# of the flower observed. We are trying to develop a machine learning algorithm that will 
# be able to classify flower species based on measurements. 

# more on this dataset: https://en.wikipedia.org/wiki/Iris_flower_data_set
# famous statistical dataset that is often used in stats classes as sample data. perfect example 
# of linear discriminant analysis.

# =========================== 02a. CODING ENVIRONMENT =========================================

# What are we trying to do? We are trying to correctly predict the species of flower based on 
# observed characteristics. 

# =========================== 02b. IMPORT PACKAGES =========================================

# TIP: Start writing your code, and then add packages here as you need them! 

# This line will let you check to see if your package is installed:
# any(grepl("<name of your package>", installed.packages()))

# The caret package provides a consistent interface into hundreds of machine learning algorithms 
# and provides useful convenience methods for data visualization, data resampling, model tuning 
# and model comparison, among other features. 
# install.packages("caret", dependencies=c("Depends", "Suggests"))
library(caret)
names(getModelInfo()) # this is an overview of the algorithms supported by caret

# Dplyr is a core package used to enable dataframe manipulation in R.
# install.packages("dplyr", dependencies=c("Depends", "Suggests"))
library(dplyr)

# Ggplot2 is a data visualization package commonly used to create customizable, presentable 
# charts and graphs in R.
# install.packages("ggplot2", dependencies=c("Depends", "Suggests"))
library(ggplot2)

# Ggvis is data visualization package that lets you create similar graphics to ggplot2, but
# these graphics can be interactive, and so are often used for webpages.
# install.packages("ggvis", dependencies=c("Depends", "Suggests"))
library(ggvis)

# These are other less important machine learning packages in R. Mainly in this tutorial we
# use caret, so don't need to below as much. 
# install.packages("class", dependencies=c("Depends", "Suggests"))
library(class)
# install.packages("gmodels", dependencies=c("Depends", "Suggests"))
library(gmodels)

# =========================== 03. IMPORT DATA ================================================
# existing dataset in R
data(iris)
dataset <- iris

# OR - if loading a file from your own computer
# dataset <- read.csv(filename, header = TRUE/FALSE)

# =========================== 04a. EXPLORE DATA ================================================

# first off, check out the variables present in the dataset
str(dataset)

# dimensions of dataset ( you can also see the dimensions in the variable explorer)
# dim(dataset)

# list types for each attribute
sapply(dataset, class)

# take a peek at the first 5 rows of the data (you can also explore the data in the variable explorer)
# head(dataset)

# list the levels for a variable
# levels(dataset$Species)

# summarize the class distribution
table(dataset$Species)
percentage <- prop.table(table(dataset$Species)) * 100
cbind(freq=table(dataset$Species), percentage=percentage)

# summarize attribute distributions
# great way to see the range of values (and potential differences in scale)
summary(dataset)

# =========================== 04b. VISUALIZE DATA ================================================

# split input and output
x <- dataset[,1:4]
y <- dataset[,5]

# boxplot for each attribute on one image (zoom to see full size)
par(mfrow=c(1,4))
for(i in 1:4) {
  boxplot(x[,i], main=names(iris)[i])}

# barplot for class breakdown (clear plot history to reset scale)
plot(y)

# scatterplot matrix
featurePlot(x=x, y=y, plot="ellipse") #caret
ggplot(dataset, aes(x = Sepal.Length, y = Sepal.Width)) +  geom_point(aes(color = factor(Species))) #ggplot2
iris %>% ggvis(~Sepal.Length, ~Sepal.Width, fill = ~Species) %>% layer_points()
iris %>% ggvis(~Petal.Length, ~Petal.Width, fill = ~Species) %>% layer_points()

# box and whisker plots for each attribute
featurePlot(x=x, y=y, plot="box")

# density plots for each attribute by class value
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)

# =========================== 04c. PROCESS DATA ================================================

####################### NORMALIZATION ####################### 
# You may need to normalize your data so that its consistent. You normalize your dataset if one 
# attribute haS a much wider range of values relative to another. This means that the distance will
# be dominated by this feature. 

normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)}

# In this case, we see that it is not necessary because the ranges of the attributes in this dataset 
# are similar.  All values of all attributes are contained within the range of 0.1 and 7.9, which we 
# consider acceptable.
summary(dataset)
# YourNormalizedDataSet <- as.data.frame(lapply(YourDataSet, normalize))

######################  TRAINING and TEST SET ####################### 
# at this point you can choose whether to split your dataset in training or test data
# create a list of 80% of the rows in the original dataset we can use for training
set.seed(1234)
validation_index <- createDataPartition(dataset$Species, p=0.80, list=FALSE)
# select 20% of the data for validation
validation <- dataset[-validation_index,]
# use the remaining 80% of data to training and testing the models
dataset <- dataset[validation_index,]

# here's another way to split the data
set.seed(1234)
ind <- sample(2, nrow(dataset), replace=TRUE, prob=c(0.80, 0.20))
iris.training <- dataset[ind==1, 1:5]
iris.test <- dataset[ind==2, 1:5]


# =========================== 05a. ALGORITHMS ==================================================

# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

# a) linear algorithms
set.seed(7)
fit.lda <- train(Species~., data=dataset, method="lda", metric=metric, trControl=control)

# b) nonlinear algorithms
# CART
set.seed(7)
fit.cart <- train(Species~., data=dataset, method="rpart", metric=metric, trControl=control)

# kNN
set.seed(7)
fit.knn <- train(Species~., data=dataset, method="knn", metric=metric, trControl=control)
# another approach ((less neat than when using the caret package))
# iris_pred <- knn(train = iris.training[,1:4], test = iris.test[,1:4], cl = iris.training[,5], k=3)
# CrossTable(x = iris.test[,5], y = iris_pred, prop.chisq=FALSE)

# c) advanced algorithms
# SVM
set.seed(7)
fit.svm <- train(Species~., data=dataset, method="svmRadial", metric=metric, trControl=control)

# Random Forest
set.seed(7)
fit.rf <- train(Species~., data=dataset, method="rf", metric=metric, trControl=control)

# =========================== 05b. RESULTS ================================================

# comprehensive overview of all machine learning packages available in R
# https://www.rdocumentation.org/taskviews/MachineLearning

# summarize accuracy of models 
# accuracy is number of instances that were classified correctly throughout the confusion matrix
# kappa measures how much better the classifier is, compared to guessing with the target distribution
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)

# compare accuracy of models
dotplot(results)

# summarize Best Model
print(fit.lda)

# estimate skill of various algorithms on the validation dataset
predictions <- predict(fit.lda, validation)
table(predictions)
confusionMatrix(predictions, validation$Species)

# =========================== 06. WHAT MORE? ================================================

# feature importance
# hyperparameter tuning
# play with various training/test splits. see how robust your results are!
# what other algorithms can you test?
# basis expansion for your features or attributes?

# =========================== 07. CLOSING REMARKS ================================================

# Check out sources provided at the top of this document to continue your learning journey!
