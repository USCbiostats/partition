#' super_partition
#'
#' @description `super_partition` implements the agglomerative, data reduction method Partition for datasets with large numbers of features by first 'super-partitioning' the data into smaller clusters to Partition.
#'
#' @param full_data sample by feature data frame or matrix
#' @param threshold information loss criterion between 0 and 1; default is 0.5
#' @param cluster_size maximum size of any single cluster; default is 4000
#' @param threshold the minimum proportion of information explained by a reduced variable; `threshold` sets a boundary for information loss because each reduced variable must explain at least as much as `threshold` as measured by the metric.
#' @param partitioner a `partitioner`. See the `part_*()` functions and [`as_partitioner()`].
#' @param tolerance a small tolerance within the threshold; if a reduction is within the threshold plus/minus the tolerance, it will reduce.
#' @param niter the number of iterations. By default, it is calculated as 20% of the number of variables or 10, whichever is larger.
#' @param x the prefix of the new variable names
#' @param .sep a character vector that separates `x` from the number (e.g. "reduced_var_1").
#' @return Partition object
#'
#' @details `super_partition` scales up partition with an approximation, using Genie, a fast, hierarchical clustering algorithm with similar qualities of those to Partition, to first super-partition the data into ceiling(N/c) clusters, where N is the number of features in the full dataset and c is the user-defined maximum cluster size (default value = 4,000). Then, if any cluster from the super-partition has a size greater than c, use Genie again on that cluster until all cluster sizes are less than c. Finally, apply the Partition algorithm to each of the super-partitions.
#'
#' It may be the case that large super-partitions cannot be easily broken with Genie due to high similarity between features. In this case, we use k-means to break the cluster.
#'
#' @examples
#'
#' set.seed(123)
#' df <- simulate_block_data(c(15, 25, 10), lower_corr = .4, upper_corr = .6, n = 100)
#'
#' #  don't accept reductions where information < .6
#' prt <- super_partition(df, threshold = .6, cluster_size = 30)
#' prt
#'
#' @references
#' Barrett, Malcolm and Joshua Millstein (2020). partition: A fast and flexible framework for data reduction in R. Journal of Open Source Software, 5(47), 1991, https://doi.org/10.21105/joss.01991Millstein J, Battaglin F, Barrett M, Cao S, Zhang W, Stintzing S, et al. Partition: a surjective mapping approach for dimensionality reduction. *Bioinformatics* **36** (2019) 676–681. doi:10.1093/bioinformatics/btz661.
#'
#' Gagolewski, Marek, Maciej Bartoszuk, and Anna Cena. "Genie: A new, fast, and outlier-resistant hierarchical clustering algorithm." Information Sciences 363 (2016): 8-23.
#'
#' Millstein, Joshua, Francesca Battaglin, Malcolm Barrett, Shu Cao, Wu Zhang, Sebastian Stintzing, Volker Heinemann, and Heinz-Josef Lenz. 2020. “Partition: A Surjective Mapping Approach for Dimensionality Reduction.” *Bioinformatics* 36 (3): https://doi.org/676–81.10.1093/bioinformatics/btz661.
#'
#' @seealso [partition()]
#'
#' @author Katelyn Queen, \email{kjqueen@@usc.edu}
#'
#' @export
#' @import progress
#' @import genieclust
super_partition <- function(full_data, threshold = 0.5, cluster_size = 4000, partitioner = part_icc(), tolerance = .0001, niter = NULL, x = "reduced_var", .sep = "_") {

  # ensure 0 < threshold < 1
  if(0 > threshold | 1 < threshold) stop("Threshold must be between 0 and 1.")

  # ensure data frame structure
  full_data <- as.data.frame(full_data)

  # if < 4,000 features, call regular partition
  if(ncol(full_data) < cluster_size) {
    message(paste0("Using regular Partition function since there are < ", cluster_size, "features."))
    return(partition(full_data, threshold = threshold))
  }

  # iteration counters
  i = j = k = l = 0

  # Function to go from cluster-specific column numbers to full data column numbers
  ## full_data  - sample by probe expression data for all probes
  ## small_data - sample by probe expression data for probes in specific cluster
  ## modules   - indices from partition mapping key
  ## return - indices from partition mapping key for full data
  full_data_col_numbers <- function(full_data, small_data, modules) {

    # vector of lists to return
    return_mods <- numeric(length(modules))

    for (i in 1:length(modules)) {
      # create vector for full data indices
      col_full <- numeric(length(unlist(modules[i])))

      # get column names
      col_small <- colnames(small_data)[unlist(modules[i])]

      # get column indices in full data
      for (j in 1:length(col_small)) {
        col_full[j] <- grep(col_small[j], colnames(full_data))
      }

      # add list of indices to return vector
      return_mods[i] <- list(col_full)
    }
  }

  # tracking variables
  master_cluster <- data.frame(col_name = colnames(full_data), cluster = 1)
  num_modules    <- 0

  # use genie (fast agglomerative hierarchical clustering) to cluster data into size 3,000 chunks
  ## transpose data since genie clusters on rows
  clust                 <- genie(t(full_data), k = ceiling(ncol(full_data)/3000), gini_threshold = 0.05)
  master_cluster$cluster <- clust
  clust_size             <- table(clust)

  # Function to continually reduce clusters until largest cluster has less than 4,000 features
  # cs - clust_size, numeric vector of cluster sizes
  # fd - full_data, sample x feature
  reduce_clust <- function(cs, fd) {
    # bookkeeping
    largest_clust <- max(master_cluster$cluster)
    unique_vals  <- unique(master_cluster$clust)
    clusters     <- master_cluster$cluster

    # keep looping until all clusters are under max cluster size
    while(max(cs) > cluster_size) {
      for (l in 1:length(cs)) {
        # if cluster size is over max cluster size...
        if(cs[l] > cluster_size) {
          # cluster again
          new_clust <- genie(t(fd[, which(clusters == unique_vals[l])]),
                            k = ceiling(cs[l]/cluster_size),
                            gini_threshold = 0.05)

          # if genie doesn't do anything, use kmeans
          if(length(unique(new_clust)) == 1 | min(table(new_clust)) < 50) {
            new_clust <- kmeans(t(fd[, which(clusters == unique_vals[l])]),
                               centers = ceiling(cs[l]/cluster_size))$cluster
          }

          # reassign cluster numbers; use <<- to rewrite data from dataframe defined outside function
          master_cluster$cluster[which(clusters == unique_vals[l])] <<- new_clust + largest_clust

          # update largest cluster number
          largest_clust <- max(master_cluster$cluster)
        }
      }

      # update bookkeeping variables
      cs           <- table(master_cluster$cluster)
      unique_vals  <- sort(unique(master_cluster$clust))
      clusters     <- master_cluster$cluster
    }
  }

  reduce_clust(cs = clust_size, fd = full_data)
  message(paste0(length(unique(master_cluster$cluster)), " super clusters identified. Beginning Partition."))
  message(paste0("Maximum cluster size: ", max(table(master_cluster$cluster))))

  # set up progress bar
  n_iter <- length(unique(master_cluster$cluster))
  pb     <- progress_bar$new(format =
                               "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                             total = n_iter,
                             complete = "=",   # Completion bar character
                             incomplete = "-", # Incomplete bar character
                             current = ">",    # Current bar character
                             clear = FALSE,    # If TRUE, clears the bar when finish
                             width = 100)

  ## first cluster
  # get initial partition to build off
  part_master <- partition(full_data[, which(master_cluster$cluster == unique(master_cluster$cluster)[1])],
                           threshold, partitioner, tolerance, niter, x, .sep)

  # update indices for each module
  mod_rows <- grep("reduced_var_", part_master$mapping_key$variable)
  suppressWarnings(part_master$mapping_key$indices <- full_data_col_numbers(full_data = full_data,
                                                                         small_data = full_data[, which(master_cluster$cluster == unique(master_cluster$cluster)[1])],
                                                                         modules = part_master$mapping_key$indices))

  # update number of modules
  num_modules  <- num_modules + length(grep("reduced_var_", part_master$mapping_key$variable))

  # create super_partition column in mapping key
  part_master$mapping_key$super_partition <- 1

  # progress bar updates
  pb$tick()

  # for each cluster...
  for (i in 2:n_iter) {

    # what to do if cluster is of size one
    if(sum(master_cluster$cluster == unique(master_cluster$cluster)[i]) == 1) {
      # cbind data to master partition reduced data
      part_master$reduced_data <- cbind(part_master$reduced_data,
                                        full_data[, which(master_cluster$cluster == unique(master_cluster$cluster)[i])])

      # add data to master partition mapping key
      name <- master_cluster$col_name[which(master_cluster$cluster == unique(master_cluster$cluster)[i])]
      part_master$mapping_key <- rbind(part_master$mapping_key,
                                       c(name, name, 1, grep(name, colnames(full_data))))

      # end iteration
      pb$tick()
      break()
    }

    ## partition
    part_clust <- partition(full_data[, which(master_cluster$cluster == unique(master_cluster$cluster)[i])],
                            threshold, partitioner, tolerance, niter, x, .sep)

    ## reduced data
    # get column indices of reduced vars
    mod_cols <- grep("reduced_var_", colnames(part_clust$reduced_data))

    # update module numbers
    for (j in 1:length(mod_cols)) {
      colnames(part_clust$reduced_data)[mod_cols[j]] <- paste0("reduced_var_", num_modules+j)
    }

    # add to master partition
    part_master$reduced_data <- cbind(part_master$reduced_data, part_clust$reduced_data)

    ## mapping key
    # get row indices of reduced vars
    mod_rows <- grep("reduced_var_", part_clust$mapping_key$variable)

    # update module number
    for (k in 1:length(mod_rows)) {
      part_clust$mapping_key$variable[mod_rows[k]] <- paste0("reduced_var_", num_modules+k)
    }

    # update indices
    suppressWarnings(part_clust$mapping_key$indices <- full_data_col_numbers(full_data = full_data,
                                                                          small_data = full_data[, which(master_cluster$cluster == unique(master_cluster$cluster)[i])],
                                                                          modules = part_clust$mapping_key$indices))

    # add super_partition column
    part_clust$mapping_key$super_partition <- i

    # add to master partition
    part_master$mapping_key <- rbind(part_master$mapping_key, part_clust$mapping_key)

    # add new modules to overall count
    num_modules <- num_modules + length(mod_cols)

    # progress bar updates
    pb$tick()
  }

  part_master <- part_master
}
