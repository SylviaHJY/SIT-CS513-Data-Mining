#################################################
#  Company    : Stevens 
#  Project    : CS513-Data Mining
#  Purpose    : hw05-CART
#  First Name  : Jiayin
#  Last Name  : Huang
#  Id			    : 10477088


rm(list=ls())
#################################################
setwd("/Users/jiayinhuang/SIT-homework/CS/cs513-homework/hw5-CART")
data <- read.csv("breast-cancer-wisconsin.csv", na.strings = "?")
View(data)

#Turn the Class column into factor value
data$Class<- factor(data$Class , levels = c(2,4) , labels = c("Benign","Malignant"))
data<-data[,-1]
head(data)
summary(data)
set.seed(222)
library(rpart)
library(rpart.plot)  			# Enhanced tree plots
library(rattle)           # Fancy tree plot
library(RColorBrewer)     # colors needed for rattle

idx<-sort(sample(nrow(data),round(.70*nrow(data))))
training<-data[idx,]
test<-data[-idx,]

CART_class<-rpart( Class~.,data=training)
rpart.plot(CART_class)
CART_predict2<-predict(CART_class,test, type="class") 
table(Actual=test[,10],CART=CART_predict2)

CART_predict<-predict(CART_class,test)
str(CART_predict)
CART_predict_cat<-ifelse(CART_predict[,1]<=.5,'Malignant','Benign')


confusion_Matrix <-table(Actual=test[,10],CART=CART_predict_cat)
#Accuracy
accuracy <- function (x) {sum(diag(x) / (sum(rowSums(x))) * 100);};
accuracy(confusion_Matrix)

CART_wrong<-sum(test[,10]!=CART_predict_cat)
CART_error_rate<-CART_wrong/length(test[,10])
CART_error_rate
CART_predict2<-predict(CART_class,test, type="class")
CART_wrong2<-sum(test[,10]!=CART_predict2)
CART_error_rate2<-CART_wrong2/length(test[,10])
CART_error_rate2 

library(rpart.plot)
?prp()
prp(CART_class)

# much fancier graph
fancyRpartPlot(CART_class)




