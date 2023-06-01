#############################################
# Company      : Stevens
# Project      : 513-Midterm
# Purpose      : Midterm #4 questions- Naïve Bayes model
# First Name   : Jiayin
# Last Name    : Huang
# Id           : 10477088

rm(list=ls())
#############################################
setwd("/Users/jiayinhuang/SIT-homework/CS/cs513-homework/Midterm")
data <- read.csv("CS513_targeting_cat_full.CSV", na.strings = "?")
View(data)

?summary
summary(data)

#delete the rows with missing value
data <- na.omit(data)
View(data)

# Preprocessing the data
data$Age <- as.factor(data$Age)
data$Gender <- as.factor(data$Gender)
data$Region <- as.factor(data$Region)
data$Product <- as.factor(data$Product)
data$Purchase <- as.factor(data$Purchase)

# Split the data into training (70%) and testing (30%) sets
# By setting the seed to a fixed number (in this case, 222), you ensure that any random operations performed in your code will produce the same results every time you run the code. 
set.seed(222)
idx <- sample(nrow(data), as.integer(.70 * nrow(data)))
trainData <- data[idx,]
testData <- data[-idx,]

# Load the 'e1071' library for the Naïve Bayes function
if (!("e1071" %in% rownames(installed.packages()))) {
  install.packages("e1071")
}
library(e1071)

# Train the Naïve Bayes model
naive_bayes_model <- naiveBayes(Purchase ~ ., data = trainData)

# Make predictions using the model
predictions <- predict(naive_bayes_model, testData)

# Evaluate the model's performance
conf_matrix <- table(Predicted = predictions, Actual = testData$Purchase)
print(conf_matrix)

# Calculate accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("Accuracy:", accuracy * 100, "%\n")

# Calculate precision, recall, specificity, and F1
true_positive <- conf_matrix[2, 2]
false_positive <- conf_matrix[1, 2]
true_negative <- conf_matrix[1, 1]
false_negative <- conf_matrix[2, 1]

precision <- true_positive / (true_positive + false_positive)
recall <- true_positive / (true_positive + false_negative)
specificity <- true_negative / (true_negative + false_positive)
f1 <- 2 * (precision * recall) / (precision + recall)

cat("Precision:", precision * 100, "%\n")
cat("Recall:", recall * 100, "%\n")
cat("Specificity:", specificity * 100, "%\n")
cat("F1 Score:", f1 * 100, "%\n")


