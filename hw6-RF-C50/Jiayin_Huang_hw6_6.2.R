#################################################
#  Company    : Stevens 
#  Project    : CS513-Data Mining
#  Purpose    : hw06——6.2-RF
#  First Name  : Jiayin
#  Last Name  : Huang
#  Id			    : 10477088


rm(list=ls())
#################################################
setwd("/Users/jiayinhuang/SIT-homework/CS/cs513-homework/hw6-RF-C50")
data <- read.csv("breast-cancer-wisconsin.csv", na.strings = "?")
View(data)

#delete the rows with missing value
data <- na.omit(data)
View(data)

library(randomForest)

set.seed(111)
#Turn the Class column into factor value
data$Class<- factor(data$Class , levels = c(2,4) , labels = c("Benign","Malignant"))

idx<-sort(sample(nrow(data),round(.30*nrow(data))))
training<-data[idx,]
test<-data[-idx,]

#6.2 Use the Random Forest methodology to develop a classification model for the Diagnosis and identify important features.
fit <- randomForest( Class~., data=training, importance=TRUE, ntree=1000)
importance(fit)
varImpPlot(fit)

#predict with test
Prediction <- predict(fit, test)
conf_matrix<-table(actual=test[,11],Prediction)

#Accuracy
accuracy <- function (x) {sum(diag(x) / (sum(rowSums(x))) * 100);};
accuracy(conf_matrix)

wrong<- (test[,11]!=Prediction )
error_rate<-sum(wrong)/length(wrong)
error_rate 

