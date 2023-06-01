rm(list=ls())

install.packages("RWeka")
library(RWeka)

filename <- file.choose()
data <- read.csv(filename, na.strings=c("?"))
View(data)

# Convert variables to factors
data$Dist_to_work <- as.factor(data$Dist_to_work)
data$Age_cat <- as.factor(data$Age_cat)
data$Abs_cat <- as.factor(data$Abs_cat)

one_level_model <- OneR(Abs_cat ~ ., data = data)
print(one_level_model)

# Create a data frame to store the rules
rules_df <- data.frame(
  Age_cat = c("Age_higher", "Age_Middle_Age", "Age_Very_young", "Age_Young"),
  Abs_cat = c("Abs_low", "Abs_low", "Abs_High", "Abs_High")
)

# Install and load the writexl package
install.packages("writexl")
library(writexl)

# Write the data frame to an Excel file in the working directory
write_xlsx(rules_df, "/Users/jiayinhuang/SIT-homework/CS/cs513-homework/FinalQ6.xlsx")

