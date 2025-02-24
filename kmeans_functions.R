# Stat 610: Statistical Computing
# K-Means Group 1 Team Members:
# Swarn Gaba (sgaba)
# Vedika Halwasiya (vhalwasi)
# Gandhar Ravindra Pansare (gpansar)


# Function 1: K-means Implementation
# Implements the K-means clustering algorithm
kmeans_custom <- function(data, k, init_centroids = NULL, max_iter = 300, tol = 1e-4) {
  
  # Ensure the data is in matrix form
  data <- as.matrix(data)
  n <- nrow(data) # Number of data points
  
  # Initialize centroids randomly if not provided
  if (is.null(init_centroids)) {
    set.seed(42) # For reproducibility
    init_centroids <- data[sample(1:n, k), ]
  }
  
  centroids <- init_centroids # Initial centroids
  history <- list() # To store the centroids and labels for each iteration
  
  for (iter in 1:max_iter) {
    
    # Step 1: Assign each point to the nearest centroid
    distances <- as.matrix(dist(rbind(centroids, data)))[1:k, (k + 1):(k + n)] # Distance matrix
    labels <- apply(distances, 2, which.min) # Assign each point to the nearest centroid
    
    # Validation: Ensure the number of labels matches the number of rows in the data
    if (length(labels) != n) {
      stop("Mismatch between number of data points and cluster labels.")
    }
    
    # Save the current centroids and labels for visualization
    history[[iter]] <- list(centroids = centroids, labels = labels)
    
    # Step 2: Update centroids by computing the mean of assigned points
    new_centroids <- t(sapply(1:k, function(i) colMeans(data[labels == i, , drop = FALSE])))
    
    # Step 3: Check for convergence (stop if centroids don't change significantly)
    if (all(sqrt(rowSums((new_centroids - centroids)^2)) < tol)) {
      break
    }
    centroids <- new_centroids # Update centroids for the next iteration
  }
  
  # Return the final centroids, labels, and history of iterations
  return(list(centroids = centroids, labels = labels, history = history))
}


# Function 2: Visualizing K-means Progress
# Visualizes the progress of the K-means algorithm with animation and static plots
visualize_kmeans_progress <- function(data, history, save_png = TRUE) {
  
  # Prepare data for animation by combining all iterations into a single data frame
  animation_data <- do.call(rbind, lapply(1:length(history), function(iter) {
    
    # Extract centroids and labels from the current iteration
    centroids <- history[[iter]]$centroids
    labels <- history[[iter]]$labels
    
    # Validation: Ensure labels match the number of rows in the data
    if (length(labels) != nrow(data)) {
      stop("Mismatch between number of labels and number of rows in the data.")
    }
    
    # Create data frames for points and centroids
    points_df <- data.frame(
      x = data[, 1],
      y = data[, 2],
      cluster = factor(labels),
      iteration = iter,
      is_centroid = FALSE
    )
    
    centroids_df <- data.frame(
      x = centroids[, 1],
      y = centroids[, 2],
      cluster = factor(1:nrow(centroids)),
      iteration = iter,
      is_centroid = TRUE
    )
    
    # Combine the points and centroids for this iteration
    combined_df <- rbind(points_df, centroids_df)
    return(combined_df)
  }))
  
  # Save each iteration as a static PNG file
  if (save_png) {
    for (iter in 1:length(history)) {
      
      iteration_data <- animation_data[animation_data$iteration == iter, ]
      
      # Create a scatter plot for the current iteration
      plot <- ggplot(iteration_data, aes(x = x, y = y, color = cluster)) +
        geom_point(aes(shape = ifelse(is_centroid, "Centroid", "Point")), size = 2) +
        scale_shape_manual(values = c("Point" = 16, "Centroid" = 4)) +
        labs(title = paste("K-means Progress: Iteration", iter), color = "Cluster", shape = "")
      
      # Save the plot as a PNG file
      ggsave(filename = paste0("kmeans_iteration_", iter, ".png"), plot = plot)
    }
  }
  
  # Create an animated plot showing the progression of K-means
  plot <- ggplot(animation_data, aes(x = x, y = y, color = cluster)) +
    geom_point(aes(shape = ifelse(is_centroid, "Centroid", "Point")), size = 2) +
    scale_shape_manual(values = c("Point" = 16, "Centroid" = 4)) +
    labs(title = "K-means Progress: Iteration {closest_state}", color = "Cluster", shape = "") +
    transition_states(iteration, transition_length = 2, state_length = 1) +
    ease_aes("linear")
  
  # Save the animation as an MP4 video
  animate(plot, nframes = length(history), fps = 1, renderer = av_renderer("kmeans_animation.mp4"))
}


# Function 3: Silhouette Score Calculation
# Computes silhouette scores to evaluate clustering quality
silhouette_custom <- function(data, labels) {
  
  data <- as.matrix(data) # Ensure the data is a matrix
  dist_matrix <- as.matrix(dist(data)) # Compute pairwise distances
  
  n <- nrow(data) # Number of data points
  sil_values <- matrix(0, nrow = n, ncol = 3) # Initialize silhouette values matrix
  
  for (i in 1:n) {
    
    current_cluster <- labels[i] # Current cluster assignment
    
    # Intra-cluster distance: Mean distance to points in the same cluster
    same_cluster_indices <- which(labels == current_cluster)
    
    if (length(same_cluster_indices) > 1) {
      a_i <- mean(dist_matrix[i, same_cluster_indices[-which(same_cluster_indices == i)]])
    } else {
      a_i <- 0
    }
    
    # Inter-cluster distance: Minimum mean distance to other clusters
    other_cluster_distances <- sapply(unique(labels[labels != current_cluster]), function(cluster) {
      other_indices <- which(labels == cluster)
      mean(dist_matrix[i, other_indices])
    })
    
    b_i <- min(other_cluster_distances)
    
    # Silhouette score for the point
    s_i <- (b_i - a_i) / max(a_i, b_i)
    
    # Store the values
    sil_values[i, ] <- c(a_i, b_i, s_i)
  }
  
  colnames(sil_values) <- c("a", "b", "s") # Assign column names
  return(sil_values)
}


# Function 4: Evaluate Clustering Quality Across k
# Evaluates silhouette scores for different values of k
evaluate_k <- function(data, k_range) {
  
  scores <- sapply(k_range, function(k) {
    # Run K-means clustering
    result <- kmeans_custom(data, k)
    
    # Compute silhouette scores
    sil_scores <- silhouette_custom(data, result$labels)
    
    # Return the mean silhouette score
    silhouette_score <- mean(sil_scores[, 3])
    return(silhouette_score)
  })
  
  # Plot silhouette scores against k
  plot(k_range, scores, type = "b", pch = 19, col = "blue",
       xlab = "Number of Clusters (k)", ylab = "Silhouette Score",
       main = "Silhouette Score vs. Number of Clusters")
  return(scores)
}
