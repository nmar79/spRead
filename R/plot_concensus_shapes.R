#' plot_concensus_shapes
#'
#' @param specimen_info_file: a data frame containing at least a specimen group assignment variable, named 'group'.
#' @param slider_file: a configuration file for semi-sliding landmarks.
#' @param landmarks_file: a TPS file 
#' @param order_vector: a vector detailing the order of the landmarks for drawing a connecting line. 
#' @return returns plots of the mean shape of each group.
#' 
#' @examples plot_concensus_shapes("sheep_specinfo_20230129.csv", "sheep_sliders_202212180739.csv", "sheep_lmrks_202212180740.TPS")
#' @export
#'

plot_concensus_shapes <- function(specimen_info_file, slider_file, landmarks_file, order_vector)
  
{
  
  require(geomorph)
  
  spec_info <- read.csv(specimen_info_file)
  spec_info$group <- factor(spec_info$group)
  lnmrks <- readland.tps(landmarks_file, specID = "ID", readcurves = TRUE)
  sliders <- read.csv(slider_file)
  gp_vole <- gpagen(lnmrks, curves = sliders)
  ngroups <- length(unique(spec_info$group))
  group_names <- unique(spec_info$group)
  group_members <- list()
  for (i in 1:ngroups) {group_members[[i]] <- which(spec_info$group == group_names[i])}
  names(group_members) <- group_names
  
  group_coords <- list()
  for (i in 1:length(group_members)){group_coords[[i]] <- gp_vole$coords[,,c(group_members[[i]])]}
  group_concensus <- list()
  lmks <- 1:length(gp_vole$coords[,1,1])
  XY <- 1:2
  spec <- 1:length(group_members)
  concensus <- array(c(lmks,XY,spec), dim = c(length(lmks),2,length(group_members)))
  for (i in 1:length(group_members)){for (j in 1:length(lmks)){concensus[j,1:2,i] <- c(mean(group_coords[[i]][j,1,]),mean(group_coords[[i]][j,2,]))}}
  
  for (i in 1:length(group_members)) {
  plot(concensus[,,i], main = paste("concensus shape",group_names[i], sep = " "))
  temp_coords <- concensus[,,i]
  new_coords <- concensus[,,i]
for (j in 1:length(lmks)){new_coords[j,] <- temp_coords[order_vector[j],]}
lines(new_coords)

}
  
  
}
  
  
