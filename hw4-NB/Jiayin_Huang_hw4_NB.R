#################################################
#  Company    : Stevens 
#  Project    : CS513-Data Mining
#  Purpose    : hw04-NB
#  First Name  : Jiayin
#  Last Name  : Huang
#  Id			    : 10477088


rm(list=ls())
#################################################
setwd("/Users/jiayinhuang/SIT-homework/CS/cs513-homework/hw4-NB")
data <- read.csv("breast-cancer-wisconsin.csv", na.strings = "?")
View(data)

#delete the rows with missing value
data <- na.omit(data)
View(data)

# Convert the 'Diagnosis Class' to a factor data type
data$Class <- factor(data$Class, levels = c(2, 4), labels = c("Benign", "Malignant"))

# Split the data into training (70%) and testing (30%) sets
# By setting the seed to a fixed number (in this case, 123), you ensure that any random operations performed in your code will produce the same results every time you run the code. 
set.seed(123)
idx <- sample(nrow(data), as.integer(.70 * nrow(data)))
trainData <- data[idx,]
testData <- data[-idx,]

# Load the 'e1071' library for the Naïve Bayes function
if (!("e1071" %in% rownames(installed.packages()))) {
  install.packages("e1071")
}
library(e1071)

# Train the Naïve Bayes model
naive_bayes_model <- naiveBayes(Class ~ ., data = trainData)

# Make predictions using the model
predictions <- predict(naive_bayes_model, testData)

# Evaluate the model's performance
conf_matrix <- table(Predicted = predictions, Actual = testData$Class)
print(conf_matrix)

# Calculate accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("Accuracy:", accuracy * 100, "%\n")
