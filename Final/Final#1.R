#################################################
#  Company    : Stevens 
#  Project    : CS513-Data Mining
#  Purpose    : Final exam #1-C5.0
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

library(C50)
set.seed(222)

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

# Build C5.0 model
C50_class <- C5.0(Abs_cat ~ .,data=training)
summary(C50_class)
plot(C50_class)

# Make predictions
C50_predict <- predict(C50_class, test, type="class")

# Compute confusion matrix
conf_matrix <- table(actual=test$Abs_cat, C50=C50_predict)
str(C50_predict)
levels(C50_predict)


# Compute accuracy
accuracy <- function (x) {sum(diag(x)) / sum(rowSums(x)) * 100}
cat("Accuracy: ", accuracy(conf_matrix), "%\n")

# Compute error rate
wrong <- sum(test$Abs_cat != C50_predict)
error_rate <- wrong / length(test$Abs_cat)
cat("Error rate: ", error_rate* 100, "%\n")

# Get precision for Abs_cat = Abs_High
abs_high_precision <- conf_matrix["Abs_High", "Abs_High"] / sum(conf_matrix[, "Abs_High"])
cat("Precision (Abs_cat = Abs_High): ", abs_high_precision* 100, "%\n")

