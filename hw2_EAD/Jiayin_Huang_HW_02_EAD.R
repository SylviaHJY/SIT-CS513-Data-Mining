#############################################
# Company      : Stevens
# Project      : 513-hw02
# Purpose      : R and Python
# First Name   : Jiayin
# Last Name    : Huang
# Id           : 10477088

rm(list=ls())
#############################################

setwd("/Users/jiayinhuang/SIT-homework/CS/cs513-homework")
data <- read.csv("breast-cancer-wisconsin.csv", na.strings = "?")
View(data)

?summary
summary(data)

# Check for missing values
sum(is.na(data))

missingDataF6 = data[which(is.na(data$F6)),]
View(missingDataF6)

# Calculate the mean of the "F6" column
mean_F6 <- mean(data$F6, na.rm = TRUE)

# Replace missing values with the mean of the "F6" column
data$F6 <- replace(data$F6, is.na(data$F6), mean_F6)
View(data)

# Create a frequency table for "Class" and "F6"
freq <- table(class=data$Class, F6=data$F6)
View(freq)

# The c() function is used to create a vector in R, and 2 and 3 are the values that make up that vector. So, in this case, c(2, 3) creates a vector with the values 2 and 3.

# When used inside of square brackets ([ ]), a vector of indices can be used to select specific columns or rows from a dataframe. 
# In the example data[c(2, 3)], this would select the 2nd and 3rd columns of the data dataframe, creating a new dataframe with only those columns.

?pairs
pairs(data[c(2,3)], main = "F1 & F2")
pairs(data[c(2,4)], main = "F1 & F3")
pairs(data[c(2,5)], main = "F1 & F4")
pairs(data[c(2,6)], main = "F1 & F5")
pairs(data[c(2,7)], main = "F1 & F6")
pairs(data[c(3,4)], main = "F2 & F3")
pairs(data[c(3,5)], main = "F2 & F4")
pairs(data[c(3,6)], main = "F2 & F5")
pairs(data[c(3,7)], main = "F2 & F6")
pairs(data[c(4,5)], main = "F3 & F4")
pairs(data[c(4,6)], main = "F3 & F5")
pairs(data[c(4,7)], main = "F3 & F6")
pairs(data[c(5,6)], main = "F4 & F5")
pairs(data[c(5,7)], main = "F4 & F6")
pairs(data[c(6,7)], main = "F5 & F6")

# Create scatter plots of each pair of columns
pairs(data[, 2:7], main = "Scatter Plots of F1 to F6")

# Create a histogram for each column
# hist() function to create a histogram for each column, with the main and xlab parameters used to customize the title and x-axis label of each plot. 
hist(data$F7, main = "Histogram of F7", xlab = "F7 Values")
hist(data$F8, main = "Histogram of F8", xlab = "F8 Values")
hist(data$F9, main = "Histogram of F9", xlab = "F9 Values")

# Create a box plot of columns F7 to F9
boxplot(data[, 8:10], main = "Box Plot of F7 to F9", xlab = "Column", ylab = "Value")

# Delete all objects from R environment
rm(list = ls())

# Load the dataset
data <- read.csv("breast-cancer-wisconsin.csv", na.strings = "?")
View(data)

# Replace missing values "?" with NA
data[data == "?"] <- NA

# Remove any rows with missing values
data <- na.omit(data)
View(data)