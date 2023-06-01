#############################################
# Company      : Stevens
# Project      : 513-Midterm
# Purpose      : Midterm #2 questions- Perform the EDA analysis
# First Name   : Jiayin
# Last Name    : Huang
# Id           : 10477088

rm(list=ls())
#############################################
setwd("/Users/jiayinhuang/SIT-homework/CS/cs513-homework/Midterm")
data <- read.csv("CS513_targeting_num.csv", na.strings = "?")
View(data)

?summary
summary(data)

#II.	Identifying missing values
missingData = data[which(is.na(data$Income)),]
View(missingData)

# Replacing the numerical missing values with the “median” 
median_Data <- median(data$Income, na.rm = TRUE)

# Replace missing values with the median of the "F6" column
data$Income <- replace(data$Income, is.na(data$Income), median_Data)
View(data)

#IV.	Displaying the scatter plot of “Age”, and “Income” 
?pairs()
pairs(data[c(2,4)], main = "Age & Income")

# V.	Show the box plots for columns: “Age” and “Income”
boxplot(data[,c(2)], main = "Box Plot of Age", xlab = "Column", ylab = "Value", names = c("Age"))

boxplot(data[,c(4)], main = "Box Plot of Income", xlab = "Column", ylab = "Value", names = c("Income"))



