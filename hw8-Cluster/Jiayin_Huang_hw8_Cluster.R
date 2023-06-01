#################################################
#  Company    : Stevens 
#  Project    : CS513-Data Mining
#  Purpose    : hw08-Cluster
#  First Name  : Jiayin
#  Last Name  : Huang
#  Id			    : 10477088


rm(list=ls())
#################################################
filename<-file.choose()
cancer<-read.csv(filename, na.strings=c("?"))
View(cancer)

library(cluster)

# Remove rows with missing values
cancer <- na.omit(cancer)

# Scale the data and remove the diagnosis column
cancer_scaled <- scale(cancer[, -which(names(cancer) %in% c("diagnosis"))])

# 8.1 Hierarchical clustering (hclust)
cancer_dist <- dist(cancer_scaled)
hclust_results <- hclust(cancer_dist)
plot(hclust_results)
hclust_2 <- cutree(hclust_results, 2)
table(hclust_2, cancer[, "diagnosis"])

# 8.2 K-means clustering
kmeans_2 <- kmeans(cancer_scaled, centers = 2, nstart = 10)
table(kmeans_2$cluster, cancer[, "diagnosis"])
kmeans_2$centers

# Function to calculate accuracy
calculate_accuracy <- function(true_labels, predicted_labels) {
  # Create a correspondence between clusters and actual diagnosis values
  confusion_matrix <- table(predicted_labels, true_labels)
  
  # Rearrange the confusion matrix to match the highest correspondence
  confusion_matrix <- confusion_matrix[, order(-colSums(confusion_matrix))]
  confusion_matrix <- confusion_matrix[order(-rowSums(confusion_matrix)), ]
  
  # Calculate accuracy
  accuracy <- sum(diag(confusion_matrix)) / sum(rowSums(confusion_matrix)) * 100
  return(accuracy)
}

# Calculate accuracy for hclust
hclust_accuracy <- calculate_accuracy(cancer[, "diagnosis"], hclust_2)
cat("Accuracy for hclust: ", hclust_accuracy, "%\n")

# Calculate accuracy for k-means
kmeans_accuracy <- calculate_accuracy(cancer[, "diagnosis"], kmeans_2$cluster)
cat("Accuracy for k-means: ", kmeans_accuracy, "%\n")

