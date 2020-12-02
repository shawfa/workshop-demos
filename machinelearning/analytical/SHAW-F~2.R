
# =========================== 00. INTRODUCTION ==================================================

# This tutorial has been developed by F. Atiyya Shaw. 
# Please direct questions to atiyya@gatech.edu.
# 12/02/2020

# Sources: 
# https://machinelearningmastery.com/machine-learning-in-r-step-by-step/
# https://www.datacamp.com/community/tutorials/machine-learning-in-r
# https://lgatto.github.io/IntroMachineLearningWithR/an-introduction-to-machine-learning-with-r.html
# https://daviddalpiaz.github.io/r4sl/the-caret-package.html

# 01. Define problem that we are trying to solve 
# 02. Discuss and prepare coding environment
# 03. Import/load + prepare data for use in coding environment
# 04. Explore + visualize + pre-process data
# 05. Run and evaluate algorithms
# 06. What's next?

# =========================== 01. PROBLEM ===================================================

# We are using the National Household Travel Survey dataset. The de-identified version of this dataset 
# is publicly available. The NHTS is a nationwide, repeated cross-sectional travel behavior survey 
# deemed the "authoritative source on travel behavior of the American public". It is funded by the 
# Federal Highway Administration. 

# Access this dataset from: https://nhts.ornl.gov/downloads

# =========================== 02a. CODING ENVIRONMENT =========================================

# What are we trying to do? We are trying to correctly predict household vehicle ownership,
# a commonly modeled outcome variable in planning and forecasting models.

# set working directory here if needed, or do it from the menu.

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

#install.packages("nnet")
library(nnet)


# =========================== 03. IMPORT DATA ================================================

# remind group to set working directory
# check to see if anyone needs more time at this point.

# load a file from your own computer
dataset <- read.csv('NHTS_hhpub.csv', header = TRUE, stringsAsFactors = TRUE, na.strings=c("", " ", "NA", NA, -8, -9, 97, -7))

# let's remove all missing data for the purposes of this demonstration (don't have time to impute or otherwise investigate missing data!)
dataset <- dataset[complete.cases(dataset), ]

# select explanatory variables to retain 
dataset <- dataset[ , which(colnames(dataset) %in% c("HHVEHCNT", "HOMEOWN", "HHSIZE", "HHFAMINC", "WRKCOUNT",
                                                     "YOUNGCHILD", "LIF_CYC", "URBANSIZE", "HBHUR", #HBHUR : urban and rural indicator for block group
                                                     "DRVRCNT", "HH_RACE", "HH_HISP"))]

# select reduced subset to use for faster computations
set.seed(1023) # set seed so you can keep track of the subset
reduced_dataset <- dataset[sample(nrow(dataset), 3000), ]

# We will model HHVEHCNT as a discrete variable, so let's record now
barplot(table(reduced_dataset$HHVEHCNT))
reduced_dataset$HHVEHCNT[reduced_dataset$HHVEHCNT >= 3] <- "3+"

# =========================== 04a. EXPLORE DATA ================================================

# first off, check out the variables present in the dataset
str(reduced_dataset)

# take a peek at the first 5 rows of the data (you can also explore the data in the variable explorer)
head(reduced_dataset)

# summarize attribute distributions
# great way to see the range of values (and potential differences in scale)
summary(reduced_dataset)

# list types for each attribute
sapply(reduced_dataset, class)

# okay, we see some variables classified as numeric that need to be reclassified into categorical variables
reduced_dataset[,c("HOMEOWN","HH_RACE","HBHUR", "HH_HISP", "HHVEHCNT")] <- lapply(reduced_dataset[,c("HOMEOWN", "HH_RACE","HBHUR", "HH_HISP","HHVEHCNT")] , factor)

# do another check to be sure things are how we want them!
sapply(reduced_dataset, class)
summary(reduced_dataset)

# =========================== 04b. VISUALIZE DATA ================================================

# split input and output
x <- reduced_dataset[,c(1,2,4:12)]
y <- reduced_dataset[,3]

# barplot for breakdown of dependent variable (clear plot history to reset scale)
barplot(table(reduced_dataset$HHVEHCNT))

# we can visually look at distributions for variables using boxplots
par(mfrow=c(1,3))
for(i in c(2,4,6)) {
  boxplot(x[,i], main=names(reduced_dataset)[i])}

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
summary(reduced_dataset) # we see that all values are contained within the range of 0 to 10 which is acceptable
# YourNormalizedDataSet <- as.data.frame(lapply(YourDataSet, normalize))

######################  TRAINING and TEST SET ####################### 

# at this point you can choose whether to split your dataset into training or test data
# create a list of 80% of the rows in the original dataset we can use for training
set.seed(1234)
validation_index <- createDataPartition(reduced_dataset$HHVEHCNT, p=0.80, list=FALSE)
# select 20% of the data for validation
validation <- reduced_dataset[-validation_index,]
# use the remaining 80% of data to training and testing the models
training <- reduced_dataset[validation_index,]

# here's another way to split the data
#set.seed(1234)
#ind <- sample(2, nrow(reduced_dataset), replace=TRUE, prob=c(0.80, 0.20))
#data.training <- reduced_dataset[ind==1, 1:12]
#data.test <- reduced_dataset[ind==2, 1:12]

# =========================== 05a. ALGORITHMS ==================================================

# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

# Baseline
# multinomial logit
set.seed(7)
fit.mnl.simple <- multinom(HHVEHCNT~., data=training)
fit.mnl <- train(HHVEHCNT~., data=training, method="multinom", metric=metric, trControl=control)


# a) linear algorithms
set.seed(7)
fit.lda <- train(HHVEHCNT~., data=training, method="lda", metric=metric, trControl=control)

# b) nonlinear algorithms
# CART
set.seed(7)
fit.cart <- train(HHVEHCNT~., data=training, method="rpart", metric=metric, trControl=control)

# kNN
set.seed(7)
fit.knn <- train(HHVEHCNT~., data=training, method="knn", metric=metric, trControl=control)

# c) advanced algorithms
# SVM
set.seed(7)
fit.svm <- train(HHVEHCNT~., data=training, method="svmRadial", metric=metric, trControl=control)

# Random Forest
set.seed(7)
fit.rf <- train(HHVEHCNT~., data=training, method="rf", metric=metric, trControl=control)

# Gradient Boosting Model
set.seed(7)
fit.gbm <- train(HHVEHCNT~., data=training, method="gbm", metric=metric, trControl=control)

# =========================== 05b. RESULTS ================================================

# comprehensive overview of all machine learning packages available in R
# https://www.rdocumentation.org/taskviews/MachineLearning

# summarize accuracy of models 
# accuracy is number of instances that were classified correctly throughout the confusion matrix
# kappa measures how much better the classifier is, compared to guessing with the target distribution
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf, gbm = fit.gbm))
summary(results)

# compare accuracy of models
dotplot(results)

# summarize best model
print(fit.gbm)
print(fit.svm)
print(fit.mnl)
print(fit.mnl.simple)

# estimate skill of various algorithms on the validation dataset
predictions <- predict(fit.gbm, validation)
table(predictions)
confusionMatrix(predictions, validation$HHVEHCNT)

predictions <- predict(fit.mnl.simple, validation)
table(predictions)
confusionMatrix(predictions, validation$HHVEHCNT)

predictions <- predict(fit.mnl, validation)
table(predictions)
confusionMatrix(predictions, validation$HHVEHCNT)

# =========================== 06. WHAT MORE? ================================================

# feature importance
# hyperparameter tuning
# play with various training/test splits. see how robust your results are!
# what other algorithms can you test?
# basis expansion for your features or attributes?

# =========================== 07. CLOSING REMARKS ================================================

# Check out sources provided at the top of this document to continue your learning journey!

# MACHINE LEARNING IS NOT A MAGIC BULLET # suited for finding patterns in LARGE amounts of features
# MACHINE LEARNING IS NOT ALWAYS NEEDED 
# MACHINE LEARNING IS NOT HARD
# DO WE SEE PUBLICATION BIASES?
# WHEN DO WE NEED MACHINE LEARNING?

