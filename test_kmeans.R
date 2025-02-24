# Stat 610: Statistical Computing
# K-Means Group 1 Team Members:
# Swarn Gaba (sgaba)
# Vedika Halwasiya (vhalwasi)
# Gandhar Ravindra Pansare (gpansar)


# Load required libraries for testing and functionality
# testthat: For unit testing
# ggplot2: For visualization (used in visualization tests)
# MASS: For data generation in tests
# gganimate: For animation testing

library(testthat)
library(ggplot2)
library(MASS)
library(gganimate)

# Load the custom K-means functions
source("kmeans_functions.R")


# Test Function 1: kmeans_custom
test_that("kmeans_custom works correctly", {
  
  # Generate random test data
  set.seed(42)
  data <- as.data.frame(matrix(runif(100), ncol = 2))  # 50 points in 2D space
  
  # Run K-means clustering with k = 3
  result <- kmeans_custom(data, k = 3)
  
  # Check if the output is a list
  expect_true(is.list(result))
  
  # Ensure the list contains required components: centroids, labels, and history
  expect_true(all(c("centroids", "labels", "history") %in% names(result)))
  
  # Validate the number of centroids matches k
  expect_equal(nrow(result$centroids), 3)
  
  # Ensure the number of labels matches the number of data points
  expect_equal(length(result$labels), nrow(data))
})


# Test Function 2: visualize_kmeans_progress
test_that("visualize_kmeans_progress works correctly", {
  
  # Generate random test data
  set.seed(42)
  data <- as.data.frame(matrix(runif(100), ncol = 2))  # 50 points in 2D space
  
  # Run K-means clustering to generate history for visualization
  result <- kmeans_custom(data, k = 3)
  
  # Ensure the visualization function runs without errors or warnings
  expect_silent(suppressMessages(visualize_kmeans_progress(data, result$history, save_png = TRUE)))
  
  # Check if PNG files are created for each iteration
  for (iter in 1:length(result$history)) {
    file_name <- paste0("kmeans_iteration_", iter, ".png")
    expect_true(file.exists(file_name), info = paste("File", file_name, "does not exist"))
  }
  
  # Check if the MP4 animation file is created
  expect_true(file.exists("kmeans_animation.mp4"), info = "File kmeans_animation.mp4 does not exist")
  
  # Cleanup: Remove all generated PNG and MP4 files after the test
  for (iter in 1:length(result$history)) {
    file_name <- paste0("kmeans_iteration_", iter, ".png")
    if (file.exists(file_name)) file.remove(file_name)
  }
  if (file.exists("kmeans_animation.mp4")) file.remove("kmeans_animation.mp4")
})


# Test Function 3: silhouette_custom
test_that("silhouette_custom works correctly", {
  
  # Generate random test data
  set.seed(42)
  data <- as.data.frame(matrix(runif(100), ncol = 2))  # 50 points in 2D space
  
  # Run K-means clustering to get labels
  result <- kmeans_custom(data, k = 3)
  
  # Compute silhouette scores
  sil_scores <- silhouette_custom(data, result$labels)
  
  # Check if the silhouette scores are returned as a matrix
  expect_true(is.matrix(sil_scores))
  
  # Validate the structure of the silhouette scores matrix (n rows, 3 columns)
  expect_equal(ncol(sil_scores), 3)
  expect_equal(nrow(sil_scores), nrow(data))
  
  # Ensure all silhouette values are within the range [-1, 1]
  expect_true(all(sil_scores[, 3] >= -1 & sil_scores[, 3] <= 1))
})


# Test Function 4: evaluate_k
test_that("evaluate_k works correctly", {
  
  # Generate random test data
  set.seed(42)
  data <- as.data.frame(matrix(runif(100), ncol = 2))  # 50 points in 2D space
  
  # Define the range of k values to evaluate
  k_range <- 2:5
  
  # Compute silhouette scores for each k
  scores <- evaluate_k(data, k_range)
  
  # Ensure the number of scores matches the length of k_range
  expect_equal(length(scores), length(k_range))
  
  # Check if the silhouette scores are numeric
  expect_true(is.numeric(scores))
})
