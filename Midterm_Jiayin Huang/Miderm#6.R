#############################################
# Company      : Stevens
# Project      : 513-Midterm
# Purpose      : Midterm #6 questions- another version of fictional customer targeting data 
# First Name   : Jiayin
# Last Name    : Huang
# Id           : 10477088

rm(list=ls())
#############################################

setwd("/Users/jiayinhuang/SIT-homework/CS/cs513-homework/Midterm")
data <- read.csv("CS513_targeting_num.CSV", na.strings = "?")
View(data)

?summary
summary(data)

#delete the rows with missing value
data <- na.omit(data)
View(data)

# Preprocessing the data
data$Age <- as.factor(data$Age)
data$Gender <- as.factor(data$Gender)
data$Income <- as.factor(data$Income)
data$Purchase <- as.factor(data$Purchase)

# Split the data into training (70%) and testing (30%) sets
# By setting the seed to a fixed number (in this case, 222), you ensure that any random operations performed in your code will produce the same results every time you run the code. 
set.seed(123)
idx <- sample(nrow(data), as.integer(.70 * nrow(data)))
trainData <- data[idx,]
testData <- data[-idx,]

?install.packages
# check to see if you have the kknn package
if (!("kknn" %in% rownames(installed.packages()))) {
  install.packages("kknn")
}
#Use the R library("kknn") 
library(kknn)
?kknn()

#run knn function for k = 3
predict_k_3 = kknn(formula =Purchase ~ ., trainData, testData, k = 3, kernel = "rectangular")
fit_3 <- fitted(predict_k_3)
conf_matrix_1 <- table(testData$Purchase, fit_3)
cat("Confusion Matrix for k=3:\n")
print(conf_matrix_1)

# Make predictions via KNN
knnPrediction <- factor(fit_3, levels = levels(testData$Purchase))

# Measure the metrics
true <- ifelse(testData$Purchase == "Yes", 1, 0)
conf_matrix <- table(Predicted = knnPrediction, True = testData$Purchase)

accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("Accuracy:", accuracy * 100, "%\n")

precision <- conf_matrix[2,2]/sum(conf_matrix[,2])
cat("Precision:", precision * 100, "%\n")

specificity <- conf_matrix[1,1]/sum(conf_matrix[,1])
cat("Specificity:", specificity * 100, "%\n")

recall <- conf_matrix[2,2]/sum(conf_matrix[2,])
cat("Recall:", recall * 100, "%\n")

f1_score <- 2 * (precision * recall) / (precision + recall)
cat("F1 Score:", f1_score * 100, "%\n")

