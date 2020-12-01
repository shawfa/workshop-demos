
# =========================== 00a. INTRODUCTION ==================================================

# This quick start guide to statistical analysis in R has been developed by F. Atiyya Shaw. 
# Please direct questions to atiyya@gatech.edu.
# 12/01/2020

# You can test R out online with R studio cloud : https://rstudio.cloud/project/381034
# To download R onto your machine, you'll need to install R, the programming language; and
# we recommend also installing RStudio, a great IDE platform for working with R.

# =========================== 00b. RESOURCES ==================================================

# FREE online resources and guides for statistics in R: 
# https://odum.unc.edu/
# https://stats.idre.ucla.edu/
# https://r4ds.had.co.nz/index.html
# https://rstudio.com/resources/cheatsheets/
# https://daviddalpiaz.github.io/appliedstats/
# https://homerhanumat.github.io/elemStats/
# https://learningstatisticswithr.com/
# https://rstudio.com/resources/webinars/a-gentle-introduction-to-tidy-statistics-in-r/
# https://www.statmethods.net/stats/descriptives.html
# http://www.r-tutor.com/
# https://bookdown.org/rdpeng/rprogdatascience/
# https://www.coursera.org/specializations/statistics
# http://www.sthda.com/english/wiki/r-software

# PAID online resources and guides for statistics: 
# https://www.pluralsight.com/search?q=R
# https://statistics.laerd.com/

# Great R books and resources (may be free through your library!)
# The R book
# R for dummies
# R for data science
# Practical data science with R
# Machine learning with R 

# =========================== 00c. GETTING STARTED TIPS =============================================

# Code structure to get started
# 01. Create an Introduction section like this one! Include author, contact info, and date
# 02. Import R packages
# 03. Import/load + prepare data for use in coding environment
# 04. Explore + visualize + pre-process data
# 05. Analysis of results (don't forget to check the assumptions!)
# 06. Decide what you'd like to do next for your data 

# Tips + Debugging
# If you get an error message, google it! 
# If you can't figure out how to do something, google it!
# Use ?function to get more information -- for example, run ?sin
# You can also navigate to Help using the menu or in the Plots window.
# Highlight line you wish to execute and use Ctrl + R to run it
# RStudio has four main windows: Source Editor, Console, Workspace Browser (and History), and Plots (and Files, Packages, Help).
# Edit the appearance of your RStudio using: Tools --> Global Options

# =========================== 01. IMPORT PACKAGES ==================================================

# This line will let you check to see if your package is installed:
# any(grepl("<name of your package>", installed.packages()))

# This line with let you install R packages and any associated dependencies (other packages needed)
# install.packages("<name of package>", dependencies=c("Depends", "Suggests")) # note: remove '<' before running!

# This line loads the R packages needed and is the only part of this section that should be uncommented
library(tidyverse) # a curated collection of awesome data science packages in R 
library (ggplot2) # the most widely used data viz package in R!
library(mosaic) # another curated quantitative package funded by NSF.

# =========================== 02. LOAD DATA ==================================================

# you can choose to include the entire directory location in file path, or simply set the file 
# path through the menu option : session --> set working directory

dataset <- read.csv("<place path to file here>", header = TRUE, stringsAsFactors = FALSE, na.strings=c("", " ", "NA", NA, -888, -999))
# only the filename/path (first parameter above) is required, the other parameters can be omitted until needed)
# the final argument allows you to let R know what values you would like to designate as missing

# library(readr) # advantages of this function is that it is faster in reading in big data!
dataset = read_csv("data/example-data.csv")

# you can also read excel files using a host of packages, but my personal preference is to use csv files
install.packages("readxl")
library("readxl")
my_data <- read_excel("my_file.xls") # xls files
my_data <- read_excel("my_file.xlsx") # xlsx files

# =========================== 03a. EXPLORE DATA ==================================================

# First up, let's use simple introductory functions to take a peek at our data

# Feel free to manually go over to the Workspace Browser to open up your data
# head(dataset, n = 10) # take a peek at the first 10 rows of the dataset
# str(dataset) # get a summary of each variable in dataset
# names(dataset) # get variable names
# dim(dataset)
# sapply(dataset, class) #list types for each attribute
# unique(dataset$variableName) # let's you see the possible values for a variable

# Here are some simple introductory ways to get started with manipulating your data

# Subsetting: many different ways to do the same thing in R!
# dataset[dataset$variableName > value, c("variableName2", "variableName3", "variableName4")]
# subset(dataset, subset = variableName > value, select = c("variableName2", "variableName3", "variableName4"))
# dataset %>% filter(variableName > value) %>% select(variableName2, "variableName3", "variableName4") # requires the dplyr package
# dataset <- dataset[which(dataset$variableName == "value"),] # use quotes if the variable is a string!
# dataset <- dataset[ , - which(colnames(dataset) %in% c("variableNamesToBeRemoved"))]

# Here are some summary statistics 

# table(dataset$variableName) # categorical variables
# table(dataset$variableName) / nrow(variableName) # gives you the proportion of each unique value for categorical variables
# mean(dataset$variableName) # numerical variable
# median(dataset$variableName) # numerical variable
# crossTabTable <- xtabs(~variableName1 + variableName2,data=dataset) # cross tabs!
# rowPerc(crossTabTable) # to compute percentages for your cross tabs 
# favstats(~variableName,data=dataset) # numeric variable # through mosaic package!

# =========================== 03b. VISUALIZE DATA ==================================================
# when we visualize our data, we are looking for outliers or unusual trends that might influence our analysis!
# if you identify any outliers or interesting relationships, investigate further!

# Histograms: for numerical variables only
hist(dataset$variableName) 
hist(dataset$variableName, xlab   = "<insert text>",main   = "<insert title>", breaks = 12, # can swap out with any number, feel free to play with it!
col = "<insert color>",border = "<insert color>")

# Bar plots: for categorical or numerical variables
barplot(table(dataset$variableName))
barplot(table(dataset$variableName),xlab   = "<insert text>", ylab   = "<insert text>", main   = "<insert title>",
col = "<insert color>", border = "<insert color>")
ggplot(data=dataset, aes(x=x, y=y)) + geom_bar(stat="identity")

# Box plot: for numerical variables, gives more information than a typical descriptive plot
boxplot(dataset$variableName)
boxplot(numericalVariableName ~ categoricalVariableName, data = dataset) # let's us compare a numerical variable for difference levels of a categorical variable
boxplot(numericalVariableName ~ categoricalVariableName, data = dataset, xlab   = "<insert text>", ylab   = "<insert text>", main   = "<insert title>",
pch = 20, cex    = 2, col    = "<insert color>", border = "<insert color>") # pch = plot character # cex = size of axis label

# Scatter plot: for numerical variables
plot(dataset$variableName, data = dataset)
plot(numericalVariableName ~ numericalVariableName, data = dataset, xlab   = "<insert text>", ylab   = "<insert text>", main   = "<insert title>",
pch = 20, cex    = 2, col    = "<insert color>") # pch = plot character # cex = size of axis label
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, color = class)) # here's an example of a ggplot scatterplot using color to differentiate between classes
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, size = class)) # here's an example of a ggplot scatterplot using size to differentiate between classes
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, alpha = class)) # here's an example of a ggplot scatterplot using shading to differentiate between classes
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, shape = class)) # here's an example of a ggplot scatterplot using shape to differentiate between classes

# Creating fitted trendlines
ggplot(data = mpg) + geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv))

# Density plot: for numeric variables
densityplot(~variableName,data=dataset, main="<insert title>", xlab="<insert text>", plot.points=FALSE)

# =========================== 03c. DATA DIAGNOSTICS ==================================================

# Based on the charts above, consider removing any outliers that you identified, or come up with a strategy
# for addressing outliers in your data

# Check collinearity and/or correlations 
# Occurs when predictor variables are highly correlated
pairs(dataset, col = "dodgerblue") # explore visually 
round(cor(dataset), 2) # pearson correlations
# binaryData_matrix <- data.matrix(binaryData) # tetrachoric correlations needed for binary data
# tetraCorr <-  tetrachoric(binaryData_matrix,correct=TRUE,smooth=TRUE,global=TRUE,weight=NULL, na.rm=TRUE,delete=TRUE)

# Remove a variable from the dataset when highly correlated with another variable
corrMatrix <- cor(dataset)
index <- findCorrelation(corrMatrix, cutoff = .7, verbose = TRUE, names = FALSE, exact = ncol(corrMatrix) < 100) # you may change the cutoff

# Check variance and possibly remove near zero variance predictors
nearZeroVariancePredictors = nearZeroVar(dataset,saveMetrics = TRUE) 
str(nearZeroVariancePredictors, vec.len=2)  
TMDData_Update7 <- TMDData_Update7[,!nearZeroVariancePredictors$zeroVar] #removing zero Variance predictors 

# Check missingness 
# https://sebastiansauer.github.io/sum-isna/
proportionMissing <- (sum(is.na(dataset))) / sum(dataset) # proportion of missing values in dataset
sapply(dataset, function(x) sum(is.na(x))) # missing values by column
dataset <- dataset[(dataset$ColumnWithMissingRates >= 0.95),] # remove cases with missing rates > 5% # requires you to have saved a column with missing rates by case first 

# =========================== 04a. ANALYZE: SIGNIFICANCE TESTING ==================================================
# Don't forget to check assumptions before analysis!

# Two sample t-test: To check differences between two groups (i.e. the categorical/factor variable has two levels)
# Assumptions: normal distribution, equal variance for each group, independence 
# https://daviddalpiaz.github.io/appliedstats/analysis-of-variance.html
t.test(numericalVariable ~ categoricalVariable, data = dataset, var.equal = TRUE)

# ANOVA: To check differences when there are more than two groups (i.e. the categorical/factor variable has more than two levels)
# ensure that the grouping variable is a factor
anovaRun = aov(numericalVariable ~ categoricalVariable, data = dataset)
summary(anovaRun)

# POST HOC testing: Once we identify statistically significant differences, we may want to next 
# identify in what direction these differences are. 
with(data, pairwise.t.test(numericalVariable, categoricalVariable, p.adj = "none")) # the adj part allows us to make Bonferroni corrections -- use BH if needed
# pairwise.t.test(coagulation$coag, coagulation$diet, p.adj = "none") # same operation as above

# TWO WAY ANOVA: for if there's more than one factor/categorical variable (but with a balanced design)
# Hypotheses: 
# There is no difference in the means of factor A
# There is no difference in means of factor B
# There is no interaction between factors A and B
summary(aov(numericalVariable ~ categoricalVariable1 * categoricalVariable2, data = dataset))
# Assumptions 
# check homogeneity of variance by #plot(anovaObject, 1)
# use Levene test to check homogeneity of variance by # leveneTest(numericalVariable ~ categoricalVariable1 * categoricalVariable2, data = dataset))
# check normality by #plot(anovaObject, 2)
# use Shapiro Wilk test to identify normality as follows:
# aov_residuals <- residuals(object = anovaObject)
# shapiro.test(x = aov_residuals )

# TUKEY POST HOC TEST for identifying direction of differences
TukeyHSD('<insert name of saved anova object>', which = "categoricalVariable2")

# Power analysis: we want to design our experiments so that we have an interesting effect size
# without spending too much money.
# insert code for power analysis

# =========================== 04b. ANALYZE: REGRESSION MODELS ==================================================
# Don't forget to check assumptions before analyses!

# Linear regression (then adding the subsequent line to the plot)
simpleLinearModel = lm(y ~ x, data = dataset)

plot(y ~ y, data = dataset, xlab = "<insert>", ylab = "<insert>", main = "<insert>",
pch  = 20, cex  = 2, col  = "color")
abline(simpleLinearModel, lwd = 3, col = "color") # add model line to scatterplot

predict(simpleLinearModel) # to get predicted values for the original dataset
fitted(simpleLinearModel) # to get predicted values for the original dataset
predict(simpleLinearModel, newdata = dataset2) # to get predicted values for a new dataset

# Regression diagnostics
vif(simpleLinearModel) # use variance inflation factor function to calculate the VIFs for each of the predictors of a model

# =========================== 04c. ANALYZE: LOGISTIC MODELS ==================================================

# binary logistic regression model
m = glm(factor(outcomeVariable)~.,data = dataset, family = "binomial")  
# summary(m) to see model summary without p-values
summary(m)
# predicting outcomes and making confusion table
predictedOutcomes <- predict(m, type = 'response')
table(dataset$outcomeVariable, predictedOutcomes > 0.5)
# another way to do the confusion tables 
confusionMatrix(table(predict(m, newdata = dataset,type="response") >= 0.5,
                      dataset$outcomeVariable == 1))

# fit measures 
loglik_null <- -0.6931*sampleSize
mod_marketshare <- glm(outcomeVariable~1, family = binomial, data = dataset)
pseudoRSq_MSbase <- 1- (logLik(m)/logLik(mod_marketshare))
pseudoRSq_Nullbase <- 1- (logLik(m)/loglik_null)
adj_pseudoRSq_Nullbase <- 1 - ((logLik(m) - '<insert number of variables>')/loglik_null)
logLik(m)
BIC(m)
AIC(m)