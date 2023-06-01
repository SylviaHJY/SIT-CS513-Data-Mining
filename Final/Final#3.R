#################################################
#  Company    : Stevens 
#  Project    : CS513-Data Mining
#  Purpose    : Final exam #3-CART
#  First Name  : Jiayin
#  Last Name  : Huang
#  Id			    : 10477088


rm(list=ls())
#################################################
#Load the data
filename <- file.choose()
data <- read.csv(filename, na.strings=c("?"))
View(data)
data <- na.omit(data)

# install.packages("ggplot2")
library(rpart)
library(rpart.plot) 
library(caret)

# Convert character variables to factor
data$Abs_cat <- as.factor(data$Abs_cat)

# Create training and testing sets
set.seed(222)
idx<-sort(sample(nrow(data),round(.70*nrow(data))))
training<-data[idx,]
test<-data[-idx,]
str(training)
str(test)

# Train CART model
cart_model <- rpart(Abs_cat ~ ., data=training, method="class")
rpart.plot(cart_model)

# Make predictions on test set
cart_predict <- predict(cart_model, test, type="class")
str(cart_predict)

# Compute confusion matrix
conf_matrix <- table(actual=test$Abs_cat,cart_predict)

# Compute accuracy
accuracy <- function (x) {sum(diag(x) / (sum(rowSums(x))) * 100)}
cat("Accuracy: ", accuracy(conf_matrix), "%\n")

# Compute error rate
wrong <- sum(test$Abs_cat != cart_predict)
error_rate <- wrong / length(test$Abs_cat)
cat("Error rate: ", error_rate* 100, "%\n")

# Get precision for Abs_cat = Abs_High
abs_high_precision <- conf_matrix["Abs_High", "Abs_High"] / sum(conf_matrix[, "Abs_High"])
cat("Precision (Abs_cat = Abs_High): ", abs_high_precision* 100, "%\n")
