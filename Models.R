library(corrplot)
library(caret)

# Importing the dataset to test on Models
pima <- read.csv("diabetes.csv", col.names=c("Pregnant","Plasma_Glucose","Dias_BP","Triceps_Skin","Serum_Insulin","BMI","DPF","Age","Diabetes"))

# Viewing the dataset
head(pima)
summary(pima)



# Applying Logistic Regression model



# Preparing the DataSet
set.seed(123)
n <- nrow(pima)
train <- sample(n, trunc(0.70*n))
pima_training <- pima[train, ]
pima_testing <- pima[-train, ]

# Training The Model
glm_fm1 <- glm(Diabetes ~., data = pima_training, family = binomial)
summary(glm_fm1)

# Updating to use only the significant variables
glm_fm2 <- update(glm_fm1, ~. - Triceps_Skin - Serum_Insulin - Age )
summary(glm_fm2)

# Plotting the new model
par(mfrow = c(2,2))
plot(glm_fm2)

# Testing the Model
glm_probs <- predict(glm_fm2, newdata = pima_testing, type = "response")
glm_pred <- ifelse(glm_probs > 0.5, 1, 0)
table(Predicted = glm_pred, Actual = pima_testing$Diabetes)

# Printing the Confusion Matrix
confusionMatrix(factor(glm_pred), factor(pima_testing$Diabetes))

#Testing the rate of Accuracy
acc_glm_fit <- confusionMatrix(factor(glm_pred), factor(pima_testing$Diabetes))$overall['Accuracy']
acc_glm_fit



# Applying Decision Trees algorithm


# Loading the required libraries
library(caret)
library(tree)
library(e1071)

# Preparing the Dataset
pima$Diabetes <- as.factor(pima$Diabetes)
set.seed(1000)
intrain <- createDataPartition(y = pima$Diabetes, p = 0.7, list = FALSE)
train <- pima[intrain, ]
test <- pima[-intrain, ]

# Training The Model
treemod <- tree(Diabetes ~ ., data = train)
summary(treemod)
treemod # get a detailed text output.

# Plotting of the tree and interpreting the results.
plot(treemod)
text(treemod, pretty = 0)

# Testing the Model
tree_pred <- predict(treemod, newdata = test, type = "class" )
confusionMatrix(tree_pred, test$Diabetes)

#Testing the rate of Accuracy
acc_treemod <- confusionMatrix(tree_pred, test$Diabetes)$overall['Accuracy']
acc_treemod



# Applying Random Forests Model



# Training The Model
library(randomForest)
set.seed(123)
rf_pima <- randomForest(Diabetes ~., data = pima_training, mtry = 8, ntree=50, importance = TRUE)

# Testing the Model
rf_probs <- predict(rf_pima, newdata = pima_testing)
rf_pred <- ifelse(rf_probs > 0.5, 1, 0)
confusionMatrix(factor(rf_pred), factor(pima_testing$Diabetes))

# Testing the rate of Accuracy
acc_rf_pima <- confusionMatrix(factor(rf_pred), factor(pima_testing$Diabetes))$overall['Accuracy']
acc_rf_pima

# Testing the importance of the variables
importance(rf_pima)

# Plotting the Variable Importance
par(mfrow = c(1, 2))
varImpPlot(rf_pima, type = 2, main = "Variable Importance",col = 'black')
plot(rf_pima, main = "Error vs no. of trees grown")



# Applying Support Vector Machine - SVM model



#Preparing the DataSet:
pima$Diabetes <- as.factor(pima$Diabetes)
set.seed(1000)
intrain <- createDataPartition(y = pima$Diabetes, p = 0.7, list = FALSE)
train <- pima[intrain, ]
test <- pima[-intrain, ]
tuned <- tune.svm(Diabetes ~., data = train, gamma = 10^(-6:-1), cost = 10^(-1:1))
summary(tuned) # to show the results

# Training The Model
svm_model  <- svm(Diabetes ~., data = train, kernel = "radial", gamma = 0.01, cost = 1) 
summary(svm_model)

# Testing the Model:
svm_pred <- predict(svm_model, newdata = test)
confusionMatrix(svm_pred, test$Diabetes)

# Testing the rate of Accuracy
acc_svm_model <- confusionMatrix(svm_pred, test$Diabetes)$overall['Accuracy']
acc_svm_model

# Comparing the Accuracy of every model
accuracy <- data.frame(Model=c("Logistic Regression","Decision Tree","Random Forest", "Support Vector Machine (SVM)"), Accuracy=c(acc_glm_fit, acc_treemod, acc_rf_pima, acc_svm_model ))
ggplot(accuracy,aes(x=Model,y=Accuracy)) + geom_bar(stat='identity') + theme_bw() + ggtitle('Comparison of Model Accuracy')

# The Decision Tree model has the lowest accuracy

