#################################################
#  Company    : Stevens 
#  Project    : CS513-Data Mining
#  Purpose    : hw03-knn
#  First Name  : Jiayin
#  Last Name  : Huang
#  Id			    : 10477088


rm(list=ls())
#################################################
setwd("/Users/jiayinhuang/SIT-homework/CS/cs513-homework/hw3-knn")
data <- read.csv("breast-cancer-wisconsin.csv", na.strings = "?")
View(data)

#delete the rows with missing value
data <- na.omit(data)
View(data)

#categories are represented by the factor data type
data$Class<- factor(data$Class , levels = c("2","4") , labels =c("Benign","Malignant"))
summary(data)

#KNN

#Generate train and test in the ratio 70% to 30%
idx <- sample(nrow(data), as.integer(.70 * nrow(data)))
trainData = data[idx,]
testData = data[-idx,]

?install.packages
# check to see if you have the kknn package
if (!("kknn" %in% rownames(installed.packages()))) {
  install.packages("kknn")
}
#Use the R library("kknn") 
library(kknn)
?kknn()

#run knn function for k = 3
predict_k_3 = kknn(formula = Class~., trainData, testData, k = 3, kernel = "rectangular")
fit_3 <- fitted(predict_k_3)
conf_matrix_3 <- table(testData$Class, fit_3)
cat("Confusion Matrix for k=3:\n")
print(conf_matrix_3)

#run knn function for k = 5
predict_k_5 = kknn(formula = Class~., trainData, testData, k = 5, kernel = "rectangular")
fit_5 <- fitted(predict_k_5)
conf_matrix_5 <- table(testData$Class, fit_5)
cat("Confusion Matrix for k=5:\n")
print(conf_matrix_5)

#run knn function for k = 10
predict_k_10 = kknn(formula = Class~., trainData, testData, k = 10, kernel = "rectangular")
fit_10 <- fitted(predict_k_10)
conf_matrix_10 <- table(testData$Class, fit_10)
cat("Confusion Matrix for k=10:\n")
print(conf_matrix_10)

#Accuracy function
accuracy <- function(x) { sum(diag(x) / sum(rowSums(x))) * 100 }

#Calculate accuracy for each k value
cat("Accuracy for k=3:", accuracy(conf_matrix_3), "%\n")
cat("Accuracy for k=5:", accuracy(conf_matrix_5), "%\n")
cat("Accuracy for k=10:", accuracy(conf_matrix_10), "%\n")






