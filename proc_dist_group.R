#' Procrustes distance between groups
#'
#' Takes as arguments a specimen information file, with a grouping column labelled "group"; a sliders file, and a .TPS file. It runs Generalized Procrustes Analysis using geomorph(), and calculates the concensus shape for each group. Then it plots a neighbour joining tree based on the Euclidean distances between the concensus shapes.
#' @param specimen_info_file A data frame with information on the group assignment of specimens in a column named "group".  
#' @param slider_file A configuration file for semi-sliding landmarks.
#' @param landmarks_file A TPS file with landmark coordinates.
#' @return A matrix with inter-centroid distances; A neighbour joining tree. 
#' @examples 
#' vole_distances <- proc_dist_group("vole_specinfo.csv", "vole_sliders.csv", "vole_lmarks.csv")
#' @export


proc_dist_group <- function(specimen_info_file, slider_file, landmarks_file)

{

require(geomorph)
require(ape)
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
nlndmrks <- length(lnmrks[,1,1])
lmks <- 1:nlndmrks
XY <- 1:2
spec <- ngroups
concensus <- array(c(lmks,XY,spec), dim = c(nlndmrks,2,ngroups))
for (i in 1:ngroups){for (j in 1:nlndmrks){concensus[j,1:2,i] <- c(mean(group_coords[[i]][j,1,]),mean(group_coords[[i]][j,2,]))}}

pr_dist <- as.matrix(dist(two.d.array(concensus)))

for (i in 1:2){dimnames(pr_dist)[[i]]<-paste(as.character(group_names, 1:nrow(pr_dist), sep=""))}

pr_dst_tree <- nj(pr_dist)

plot(pr_dst_tree, "u")
return(pr_dist)
}

