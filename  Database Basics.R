library(corrplot)
library(caret)
library(e1071)
library(readr)
library(ggplot2)
library(dplyr)


#Assigning the name "Diabetes" to the Comma Separated Variable Dataset collected from PIMA Indians Diabetes Database
diabetes <- read.csv("diabetes.csv")
View(diabetes)

#Showing the linear dimensions of the dataset
dim(diabetes)

#Showing the 2-dimensional Structure of the dataset
str(diabetes)

#Displaying the initial 6 records of the Diabetes Dataset
head(diabetes)

#Showing the summary of every variable(attribute) of the Dataset
summary(diabetes)

#Producing the matrix of scatterplots
pairs(diabetes, panel = panel.smooth)

#Checking the no. of missing values in each column and the dataset
sapply(diabetes, function(x) sum(is.na(x)))
cat("Number of missing value:", sum(is.na(diabetes)), "\n")

#Computing the matrix of correlations between the variables
corrplot(cor(diabetes[, -9]), type = "full", method = "number")
diabetes$Outcome <- factor(diabetes$Outcome)
numeric.var <- sapply(diabetes, is.numeric)
corr.matrix <- cor(diabetes[,numeric.var])

corrplot(corr.matrix , main = "Correlation Plot for Numerical Variables", order = "hclust", tl.col = "blue", tl.srt=45, tl.cex=0.5, cl.cex=0.5)

#Computing correlation between numeric variables and outcome
attach(diabetes)
par(mfrow=c(2,4))
boxplot(Pregnancies~Outcome, main="No. of Pregnancies vs. Diabetes", xlab="Outcome", ylab="Pregnancies")
boxplot(Glucose~Outcome, main="Glucose vs. Diabetes", xlab="Outcome", ylab="Glucose")
boxplot(BloodPressure~Outcome, main="Blood Pressure vs. Diabetes", xlab="Outcome", ylab="Blood Pressure")
boxplot(SkinThickness~Outcome, main="Skin Thickness vs. Diabetes", xlab="Outcome", ylab="Skin Thickness")
boxplot(Insulin~Outcome, main="Insulin vs. Diabetes", xlab="Outcome", ylab="Insulin")
boxplot(BMI~Outcome, main="BMI vs. Diabetes", xlab="Outcome", ylab="BMI")
boxplot(DiabetesPedigreeFunction~Outcome, main="Diabetes Pedigree Function vs. Diabetes", xlab="Outcome", ylab="DiabetesPedigreeFunction")
boxplot(Age~Outcome, main="Age vs. Diabetes", xlab="Outcome", ylab="Age")
#Blood pressure and skin thickness show little variation with diabetes, they will be excluded from the model. Other variables show more or less correlation with diabetes, so will be kept


