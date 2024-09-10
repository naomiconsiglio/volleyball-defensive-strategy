library(dplyr)
library(plyr)
library(readr)
library(data.table)
library(MASS)

calc_coef<- function(prob_0, prob_max_dist, max_dist){
  alpha <- prob_0
  beta <- (log(prob_max_dist/alpha))/(max_dist^2)
  calc_prob <- function(distance, alpha_val = alpha, beta_val = beta){
    dig_prob <- alpha_val*exp(beta_val*distance^2)
    return(dig_prob)
  }
  
  return(list(alpha = alpha, beta = beta))
}

pin_hard_attacks <- calc_coef(0.80, 0.10, 0.7)
pin_soft_attacks <- calc_coef(0.90, 0.10, 1)
middle_hard_attacks <- calc_coef(0.50, 0.10, .4)
middle_soft_attacks <- calc_coef(0.75, 0.10, .6)

calc_coef_plot<- function(prob_0, prob_max_dist, max_dist){
  alpha <- prob_0
  beta <- (log(prob_max_dist/alpha))/(max_dist^2)
  calc_prob <- function(distance, alpha_val = alpha, beta_val = beta){
    dig_prob <- alpha_val*exp(beta_val*distance^2)
    return(dig_prob)
  }
  
  distance_grid <- seq(from = 0, to = 12, by = 0.1)
  
  plot(
    x = distance_grid,
    y = calc_prob(distance_grid),
    type = 'l',
    xlab = 'Distance in Feet',
    ylab = 'Dig Probability',
    main = 'Middle Soft Attacks'
  )
  return(list(alpha = alpha, beta = beta))
}
calc_coef_plot(0.75, 0.10, 6)

# Function to calculate objective function value
objective_function <- function(alpha, beta, x, y, x_0, y_0) {
  return(alpha * exp(beta * ((x - x_0)^2 + (y - y_0)^2)))
}


# Function to perform gradient descent to update coordinates (x_0, y_0)
gradient_descent <- function(alpha, beta, x, y, x_0, y_0, step_size = 0.1, tolerance = 0.0001) {
  
  prev_obj <- objective_function(alpha, beta, x, y, x_0, y_0)
  
  while (TRUE) {
    # Compute gradient
    df_dx0 <- mean(2 * alpha * beta * (x_0 - x) * exp(beta * ((x - x_0)^2 + (y - y_0)^2)))
    df_dy0 <- mean(2 * alpha * beta * (y_0 - y) * exp(beta * ((x - x_0)^2 + (y - y_0)^2)))
    
    
    # Update coordinates using gradient descent rule
    x_0_new <- x_0 + step_size * df_dx0
    y_0_new <- y_0 + step_size * df_dy0
    
    # Calculate the change in objective function
    obj_new <- objective_function(alpha, beta, x, y, x_0_new, y_0_new)
    obj_change <- abs(obj_new - prev_obj)
    
    # Check for convergence
    if (all(obj_change < tolerance)) {
      break
    }
    
    # Update coordinates and objective value for the next iteration
    x_0 <- x_0_new
    y_0 <- y_0_new
    prev_obj <- obj_new
  }
  
  return(list(x_0 = x_0, y_0 = y_0))
}

distance <- function(x1, x2, y1, y2) {
  return(sqrt((x2 - x1)^2 + (y2 - y1)^2))
}

# Assign each data point to the nearest cluster center
assign_clusters <- function(data, centers) {
  clusters <- rep(NA, nrow(data))
  for (i in 1:nrow(data)) {
    distances <- apply(centers, 1, function(c) distance(data[i,1], c[1], data[i,2], c[2]))
    clusters[i] <- which.min(distances)
  }
  return(clusters)
}



# Update centers based on the mean of data points in each cluster
update_centers <- function(data, clusters, k, initial_centers, alpha, beta, step_size = 0.01, tolerance = 0.0001) {
  new_centers <- matrix(NA, nrow = k, ncol = 2)
  for (i in 1:k) {
    cluster_points <- data[clusters == i, 1:2]
    if (nrow(cluster_points) > 0) {
      x <- cluster_points[, 1]
      y <- cluster_points[, 2]
      center_coord <- gradient_descent(alpha[clusters == i], beta[clusters == i], x, y, initial_centers[i,1], initial_centers[i,2], step_size, tolerance)
      new_centers[i, ] <- c(center_coord$x_0, center_coord$y_0)
    }
  }
  return(new_centers)
}


# K-means clustering algorithm with probabilities
kmeans_probs <- function(data, k, max_iter = 100, tol = 1e-6, step_size = 0.01, tolerance = 0.0001) {
  # Pick centers randomly
  centers <- data[sample(nrow(data), k), 1:2]
  
  for (iter in 1:max_iter) {
    
    # Assign each data point to the nearest center
    clusters <- assign_clusters(data, centers)
    
    # Create alpha and beta vectors
    alpha <- ifelse(
      data$attack_type %in% c('hard attack'), 
      pin_hard_attacks$alpha, 
      pin_soft_attacks$alpha
    )
    
    beta <- ifelse(
      data$attack_type %in% c('hard attack'), 
      pin_hard_attacks$beta, 
      pin_soft_attacks$beta
    )
    
    # Update centers
    new_centers <- update_centers(data, clusters, k, centers, alpha, beta, step_size, tolerance)
    
    # Check for convergence
    obj_changes <- colSums(abs(new_centers - centers))
    if (max(obj_changes) < tol) {
      break
    }
    
    centers <- new_centers
  }
  
  return(list(clusters = clusters, centers = centers))
}


results <- kmeans_probs(oh_coordinates, k = 4)
clusters_1 <- results$clusters
centers_1 <- results$centers


outside_altered_kmeans<-kmeans_probs(oh_coordinates, k=4)
# Adjusted bounced balls and dug balls
kde2d(x = oh_coordinates$end_coordinate_x, y = oh_coordinates$end_coordinate_y, h = 0.3, n = 400, lims = c(0,4,0,7)) |> 
  image(col = viridis::viridis_pal()(400))
title(main='Outside Attacks')
points(x= outside_altered_kmeans$centers[,1], y = outside_altered_kmeans$centers[,2], col = 'white', pch = 16)
# Draw horizontal lines
segments(x0 = 0.5, x1 = 3.5, y0 = c(0.5, 2.5, 3.5, 4.5, 6.5), y1 = c(0.5, 2.5, 3.5, 4.5, 6.5), col = 'white', lwd = 2)
# Draw vertical lines
segments(x0 = c(0.5, 3.5), x1 = c(0.5, 3.5), y0 = 0.5, y1 = 6.5, col = 'white', lwd = 2)




rightside_altered_kmeans<-kmeans_probs(rh_coordinates, k=4)
# Adjusted bounced balls and dug balls
kde2d(x = rh_coordinates$end_coordinate_x, y = rh_coordinates$end_coordinate_y, h = 0.3, n = 400, lims = c(0,4,0,7)) |> 
  image(col = viridis::viridis_pal()(400))
title(main='Rightside Attacks')
points(x= rightside_altered_kmeans$centers[,1], y = rightside_altered_kmeans$centers[,2], col = 'white', pch = 16)
# Draw horizontal lines
segments(x0 = 0.5, x1 = 3.5, y0 = c(0.5, 2.5, 3.5, 4.5, 6.5), y1 = c(0.5, 2.5, 3.5, 4.5, 6.5), col = 'white', lwd = 2)
# Draw vertical lines
segments(x0 = c(0.5, 3.5), x1 = c(0.5, 3.5), y0 = 0.5, y1 = 6.5, col = 'white', lwd = 2)



# K-means clustering algorithm with probabilities
kmeans_for_middle <- function(data, k, max_iter = 100, tol = 1e-6, step_size = 0.01, tolerance = 0.0001) {
  # Pick centers randomly
  centers <- data[sample(nrow(data), k), 1:2]
  
  for (iter in 1:max_iter) {
    
    # Assign each data point to the nearest center
    clusters <- assign_clusters(data, centers)
    
    # Create alpha and beta vectors
    alpha <- ifelse(
      data$attack_type %in% c('hard attack'), 
      middle_hard_attacks$alpha, 
      middle_soft_attacks$alpha
    )
    
    beta <- ifelse(
      data$attack_type %in% c('hard attack'), 
      middle_hard_attacks$beta, 
      middle_soft_attacks$beta
    )
    
    # Update centers
    new_centers <- update_centers(data, clusters, k, centers, alpha, beta, step_size, tolerance)
    
    # Check for convergence
    obj_changes <- colSums(abs(new_centers - centers))
    if (max(obj_changes) < tol) {
      break
    }
    
    centers <- new_centers
  }
  
  return(list(clusters = clusters, centers = centers))
}


middle_altered_kmeans<-kmeans_for_middle(mb_coordinates, k=3)
# Adjusted bounced balls and dug balls
kde2d(x = mb_coordinates$end_coordinate_x, y = mb_coordinates$end_coordinate_y, h = 0.3, n = 400, lims = c(0,4,0,7)) |> 
  image(col = viridis::viridis_pal()(400))
title(main='Middle Attacks')
points(x= middle_altered_kmeans$centers[,1], y = middle_altered_kmeans$centers[,2], col = 'white', pch = 16)
# Draw horizontal lines
segments(x0 = 0.5, x1 = 3.5, y0 = c(0.5, 2.5, 3.5, 4.5, 6.5), y1 = c(0.5, 2.5, 3.5, 4.5, 6.5), col = 'white', lwd = 2)
# Draw vertical lines
segments(x0 = c(0.5, 3.5), x1 = c(0.5, 3.5), y0 = 0.5, y1 = 6.5, col = 'white', lwd = 2)



mb_clusters<-kmeans(data.frame(combined_mb$end_coordinate_x, combined_mb$end_coordinate_y), 4)
# Adjusted bounced balls and dug balls
kde2d(x = combined_mb$end_coordinate_x, y = combined_mb$end_coordinate_y, h = 0.3, n = 400, lims = c(0,4,0,7)) |> 
  image(col = viridis::viridis_pal()(400))
title(main='Middle Attacks')
points(x= mb_clusters$centers[,1], y = mb_clusters$centers[,2], col = 'white', pch = 16)
# Draw horizontal lines
segments(x0 = 0.5, x1 = 3.5, y0 = c(0.5, 2.5, 3.5, 4.5, 6.5), y1 = c(0.5, 2.5, 3.5, 4.5, 6.5), col = 'white', lwd = 2)
# Draw vertical lines
segments(x0 = c(0.5, 3.5), x1 = c(0.5, 3.5), y0 = 0.5, y1 = 6.5, col = 'white', lwd = 2)
