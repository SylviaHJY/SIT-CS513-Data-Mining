#################################################
#  Company    : Stevens 
#  Project    : CS513-Data Mining
#  Purpose    : hw06——6.1-C5.0
#  First Name  : Jiayin
#  Last Name  : Huang
#  Id			    : 10477088


rm(list=ls())
#################################################
setwd("/Users/jiayinhuang/SIT-homework/CS/cs513-homework/hw6-RF-C50")
data <- read.csv("breast-cancer-wisconsin.csv", na.strings = "?")
View(data)

#import C50
#install.packages("C50");
library(C50)
set.seed(222)
#Turn the Class column into factor value
data$Class<- factor(data$Class , levels = c(2,4) , labels = c("Benign","Malignant"))

idx<-sort(sample(nrow(data),round(.30*nrow(data))))
training<-data[idx,]
test<-data[-idx,]

#6.1 Use the C5.0 methodology to develop a classification model for the Diagnosis.  
C50_class <- C5.0(Class~.,data=training )
summary(C50_class )
plot(C50_class)

#predict with test
C50_predict<-predict( C50_class ,test , type="class" )

#Forming the confusin matrix
conf_matrix<-table(actual=test[,11],C50=C50_predict)
str(C50_predict)
#Accuracy
accuracy <- function (x) {sum(diag(x) / (sum(rowSums(x))) * 100);};
accuracy(conf_matrix)

#Showing the error rate 
wrong<-sum(test[,11]!=C50_predict)
error_rate<-wrong/length(test[,11])
error_rate


