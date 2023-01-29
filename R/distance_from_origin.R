#' Distance from origin
#'
#' Estimate the distance between the centroids of each group of specimens and the origin of the axes system. The coordinates of the centroid are calculated by sampling with replacement 80% of the observations.
#' @param data A data frame with specimens in rows; first column with group assignment; next columns with ordination scores. 
#' @param bs Number of bootstrap samples
#' @return A box plot comparing the bootstrapped estimates of the distance from the origin of the centroids of each group; a one-way ANOVA result.
#' @examples 
#' distances <- distance_from_origin(spec_info, 100);
#' @export


distance_from_origin <- function(data, bs) {

sample_my_data <- function(df) {
     sample_index <- sample.int(length(df[, 1]), floor(0.8 * length(df[, 1])), replace = TRUE)
     sampled_df <- df[sample_index, 2:ncol(data)]
     return(sampled_df)
}

calc_centre <- function(sampled_df) {
  for (i in 1:ncol(sampled_df)) {
    centre[i] <- mean(sampled_df[,i])
  }
  return(centre)
}

dist_centre <- function(centre) {
  origin <- rep(0, length(centre))
  group_from_origin <- sqrt(sum((centre - origin)^2))
  return(group_from_origin)
}


data$group <- factor(data$group)

data_by_group <- split.data.frame(data, data$group)

distances_group <- numeric()
distance_list <- list()
centre <- numeric()
dist_fr_or <- numeric()
group_names <- levels(data$group)
name_string <- character()
group_centres <- list()

for (i in 1:length(data_by_group)) {
  
  for (j in 1:bs) {
    
    sampled <- sample_my_data(data_by_group[[i]])
    centre <- calc_centre(sampled)
    dist_fr_or <- dist_centre(centre)
    distances_group[j] <- dist_fr_or
  }
  
  distance_list[[i]] <- distances_group
  
  }

names(distance_list) <- group_names
boxplot(distance_list, ylab = "distance from origin")

for (i in 1:length(data_by_group)) {
  name_string <- c(name_string, rep(group_names[i], bs))
  }
long_data <- unlist(distance_list)
long_df <- cbind.data.frame(name_string, long_data)
long_df$name_string <- factor(long_df$name_string)
aov_results <- summary(aov(long_data ~ name_string, long_df))
return(aov_results)

}
