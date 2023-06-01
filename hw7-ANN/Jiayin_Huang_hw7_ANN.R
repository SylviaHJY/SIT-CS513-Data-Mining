#################################################
#  Company    : Stevens 
#  Project    : CS513-Data Mining
#  Purpose    : hw07-ANN
#  First Name  : Jiayin
#  Last Name  : Huang
#  Id			    : 10477088


rm(list=ls())
#################################################
# Load necessary libraries
library(neuralnet)

# Read the data
setwd("/Users/jiayinhuang/SIT-homework/CS/cs513-homework/hw7-ann")
dataSet <- read.csv("wisc_bc_ContinuousVar.csv",na.strings = '?')
View(dataSet)

# Convert the diagnosis column to numeric (Malignant = 1, Benign = 0)
dataSet$diagnosis <- as.numeric(dataSet$diagnosis == "M")

# Convert the dataset to a data frame with numeric values and remove missing values (NA)
dataSet <- data.frame(lapply(na.omit(dataSet), as.numeric))

# Split the dataset into training and testing sets (70% training, 30% testing)
idx <- sort(sample(nrow(dataSet), as.integer(0.7 * nrow(dataSet))))
training <- dataSet[idx, ]
test <- dataSet[-idx, ]

# Create the neural network model with 5 hidden nodes and a threshold of 0.01
model <- neuralnet(diagnosis ~ ., training[-1], hidden = 5, threshold = 0.01)

# Plot the neural network
plot(model)

# Use the neural network model to make predictions on the test set
ann <- compute(model, test)

# Convert the predicted results to categories (Malignant = 1, Benign = 0)
ann_cat <- ifelse(ann$net.result >= 0.5, 1, 0)
length(ann_cat)
length(test$diagnosis)
# Create a confusion matrix to compare the predicted results with the actual results
confusion_matrix <- table(ann_cat, test$diagnosis)
accuracy <- function(x) {sum(diag(x)) / sum(rowSums(x)) * 100;}
accuracy(confusion_matrix)

#error rate
wrong<- (test$diagnosis!=ann_cat)
errorRate<-sum(wrong)/length(wrong)
