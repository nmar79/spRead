#a function to plot bootstrapped median distances
#between specimens insgroup_ide groups.
#arguments: 'data' = a data frame with specimen
#'group' factor and associated principle component scores;
#'bs' = number of bootstrap replications.
#value: boxplot of bootstrapped medians, anova and posthoc results

#' Distance from origin
#'
#' Returns the median distance between specimens within each group. The statistic is estimated by sampling with replacement 80% of the observations.
#' @param data A data frame with specimens in rows; first column with group assignment; next columns with ordination scores. 
#' @param bs Number of bootstrap samples
#' @return A box plot comparing the bootstrapped estimates of the median distance between specimens of each group; a one-way ANOVA result.
#' @examples 
#' medians <- within_group_distances(spec_info, 100)
#' @export

within_group_distances <- function(data, bs) {

nspecs <- length(data$group)    # total sample size
group_pcs <- split.data.frame(data, data$group)   # list of data by 'group'
ngroups <- length(group_pcs)    # number of groups
group_names <- names(group_pcs) # group names
bs_medians_group <- 1:nspecs    # bucket for bootstrap medians for each group
median_list <- list()   # list of bootstrap medians by group
group_id <- list() # list of group ids, matches 'median_list'

for (i in 1:ngroups){   # loop through each group

context <- group_pcs[[i]]   #moniker for looped group
sample_size <- length(context[, 1]) #length of looped group
boot_parameter <- round(0.8 * sample_size)
# number of sampled specimens from each group, defined as 80% of total
temp_1 <- 1:sample_size # bucket for rows chosen by sample()
temp_2 <- 1:6
# bucket for the summary() of the dist() object (gives 6 parameters)
bs_medians_group <- 1:bs # number of bootstrapped medians

# bootstrapping loop for each group
for (j in 1:bs) {
  temp_1 <- sample.int(sample_size, boot_parameter, replace = TRUE)
  # choose 80% of the rows w/ replacement
  temp_2 <- summary(dist(context[temp_1, ])) # summarise the distance matrix
  bs_medians_group[j] <- as.numeric(temp_2[3])
  # add the median from the summary to a vector
  group_id[[i]] <- rep(group_names[[i]], bs)
  # add a bs-long vector of identifiers
}

median_list[[i]] <- bs_medians_group
#builds a list of the bs medians for each group

}

medians_vct <- unlist(median_list) #flatten median list
names_vct <- unlist(group_id)   #flatten group identified list
long_data <- cbind.data.frame(as.factor(names_vct), medians_vct) # create df
between_groups <- summary(aov(medians_vct ~ names_vct, data = long_data)) # anova
boxplot(medians_vct ~ names_vct, data = long_data,
xlab = "contexts", ylab = "bootstrapped medians")

return(between_groups)

}
