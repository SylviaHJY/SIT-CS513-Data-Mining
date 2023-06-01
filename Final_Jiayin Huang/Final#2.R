#################################################
#  Company    : Stevens 
#  Project    : CS513-Data Mining
#  Purpose    : Final exam #2-Random Forest
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

library(randomForest)
set.seed(123)

# Convert character variables to factor
data$Trans_expense_cat <- as.factor(data$Trans_expense_cat)
data$Dist_to_work <- as.factor(data$Dist_to_work)
data$Age_cat <- as.factor(data$Age_cat)
data$Abs_cat <- as.factor(data$Abs_cat)

# Create training and testing sets
idx<-sort(sample(nrow(data),round(.70*nrow(data))))
training<-data[idx,]
test<-data[-idx,]
str(training)
str(test)

# Build random forest model
rf_model <- randomForest(Abs_cat ~ ., data = training, importance=TRUE, ntree = 500)
importance(rf_model)
varImpPlot(rf_model)

# Make predictions
Prediction <- predict(rf_model, newdata = test)

# Compute confusion matrix
conf_matrix <- table(actual = test$Abs_cat, Prediction)

# Compute accuracy
accuracy <- function (x) {sum(diag(x) / (sum(rowSums(x))) * 100)}
cat("Accuracy: ", accuracy(conf_matrix), "%\n")

# Compute error rate
wrong <- sum(test$Abs_cat != Prediction)
error_rate <- wrong / length(test$Abs_cat)
cat("Error rate: ", error_rate* 100, "%\n")

# Get precision for Abs_cat = Abs_High
abs_high_precision <- conf_matrix["Abs_High", "Abs_High"] / sum(conf_matrix[, "Abs_High"])
cat("Precision (Abs_cat = Abs_High): ", abs_high_precision* 100, "%\n")

