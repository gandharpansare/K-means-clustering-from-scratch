# Stat 610: Statistical Computing
# K-Means Group 1 Team Members:
# Swarn Gaba (sgaba)
# Vedika Halwasiya (vhalwasi)
# Gandhar Ravindra Pansare (gpansar)

# Purpose: This script integrates the custom K-means functions, runs clustering on the sample dataset, 
# visualize the clustering progress and Evaluate the clustering quality for different values of k

# Load required libraries
# MASS: For generating multivariate normal data
# ggplot2: For creating static and animated visualizations
# gganimate: For creating animations of the K-means clustering process

library(MASS)
library(ggplot2)
library(gganimate)

# Source the file containing the K-means functions
# This includes kmeans_custom, visualize_kmeans_progress, silhouette_custom, and evaluate_k
source("kmeans_functions.R")

# Generate sample data
# Create two clusters of 300 points each from a multivariate normal distribution

# Cluster 1: Centered at (0, 0) with a diagonal covariance matrix
set.seed(42) # Set seed for reproducibility
data <- as.data.frame(mvrnorm(300, mu = c(0, 0), Sigma = diag(2)))

# Cluster 2: Centered at (3, 3) with the same covariance matrix
data <- rbind(data, as.data.frame(mvrnorm(300, mu = c(3, 3), Sigma = diag(2))))

# Run K-means clustering on the data
# k = 2 specifies that we are looking for 2 clusters
result <- kmeans_custom(data, k = 2)

# Visualize the progress of the K-means algorithm
# This generates:
# - Static PNG files for each iteration
# - An MP4 animation showing the clustering progress
visualize_kmeans_progress(data, result$history)

# Evaluate the clustering quality for different values of k
# k_range specifies the range of k values to evaluate (from 2 to 10 clusters)
k_range <- 2:10
silhouette_scores <- evaluate_k(data, k_range)

# Print silhouette scores for each value of k
# Higher silhouette scores indicate better clustering quality
print(silhouette_scores)
