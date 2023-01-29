#' Calculate the angle between groups 
#'
#' Calculates the angle between the centres of two or more groups of specimens. 
#' @param df A data frame with specimen grouping variable (as character string) in the first column, and with PCA scores in the rest of the columns. 
#' @return A table of the angles between any two group centroids in the sample and a heat map of the values in the table. 
#' @examples 
#' angles <- angle_between_centroids(spec_info)
#' @export

angle_between_centroids <- function(df){

calc_centre <- function(df) {
  for (i in 1:ncol(df)) {
    centre[i] <- mean(df[,i])
  }
  return(centre)
}
angle_2 <- function(x,y){
  dot_prod <- x%*%y 
  norm_x <- norm(x,type="2")
  norm_y <- norm(y,type="2")
  angle <- acos(dot_prod / (norm_x * norm_y))
  as.numeric(angle)
}

df_groups <- split.data.frame(df, df$group)
group_names <- unique(df$group)
centre <- list()

for (i in 1:length(df_groups)) {
  temp_df <- df_groups[[i]][, -1]
  centre[[i]] <- calc_centre(temp_df)
  }

names(centre) <- group_names
centre <- lapply(centre, unlist)

out <- array(length(centre)^2, dim = c(length(centre), length(centre)), dimnames = list(names(centre), names(centre)))

for (i in 1:length(centre)) {
  for (j in seq_along(1:length(centre))) {
  out[i, j] <- round(angle_2(centre[[i]],centre[[j]]), digits = 3)
  }
}

heatmap(out)
return(out)
}
