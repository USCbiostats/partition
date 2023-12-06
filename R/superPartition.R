#' superPartition
#'
#' @description `superPartition` implements the agglomerative, data reduction method Partition for datasets with large numbers of features by first 'super-partitioning' the data into smaller clusters to Partition.
#'
#' @param fullData sample by feature data frame or matrix
#' @param threshold information loss criterion between 0 and 1; default is 0.5
#' @param clusterSize maximum size of any single cluster; default is 4000
#' @param threshold the minimum proportion of information explained by a reduced variable; `threshold` sets a boundary for information loss because each reduced variable must explain at least as much as `threshold` as measured by the metric.
#' @param partitioner a `partitioner`. See the `part_*()` functions and [`as_partitioner()`].
#' @param tolerance a small tolerance within the threshold; if a reduction is within the threshold plus/minus the tolerance, it will reduce.
#' @param niter the number of iterations. By default, it is calculated as 20% of the number of variables or 10, whichever is larger.
#' @param x the prefix of the new variable names
#' @param .sep a character vector that separates `x` from the number (e.g. "reduced_var_1").
#' @return Partition object
#'
#' @details `superPartition` scales up partition with an approximation, using Genie, a fast, hierarchical clustering algorithm with similar qualities of those to Partition, to first super-partition the data into ceiling(N/c) clusters, where N is the number of features in the full dataset and c is the user-defined maximum cluster size (default value = 4,000). Then, if any cluster from the super-partition has a size greater than c, use Genie again on that cluster until all cluster sizes are less than c. Finally, apply the Partition algorithm to each of the super-partitions.
#'
#' It may be the case that large super-partitions cannot be easily broken with Genie due to high similarity between features. In this case, we use k-means to break the cluster.
#'
#' @examples
#'
#' set.seed(123)
#' df <- simulate_block_data(c(15, 25, 10), lower_corr = .4, upper_corr = .6, n = 100)
#'
#' #  don't accept reductions where information < .6
#' prt <- superPartition(df, threshold = .6, clusterSize = 30)
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
superPartition <- function(fullData, threshold = 0.5, clusterSize = 4000, partitioner = part_icc(), tolerance = .0001, niter = NULL, x = "reduced_var", .sep = "_") {

  # ensure 0 < threshold < 1
  if(0 > threshold | 1 < threshold) stop("Threshold must be between 0 and 1.")

  # ensure data frame structure
  fullData <- as.data.frame(fullData)

  # if < 4,000 features, call regular partition
  if(ncol(fullData) < clusterSize) {
    message(paste0("Using regular Partition function since there are < ", clusterSize, "features."))
    return(partition(fullData, threshold = threshold))
  }

  # iteration counters
  i = j = k = l = 0

  # Function to go from cluster-specific column numbers to full data column numbers
  ## fullData  - sample by probe expression data for all probes
  ## smallData - sample by probe expression data for probes in specific cluster
  ## modules   - indices from partition mapping key
  ## return - indices from partition mapping key for full data
  fullDataColNumbers <- function(fullData, smallData, modules) {

    # vector of lists to return
    returnMods <- numeric(length(modules))

    for (i in 1:length(modules)) {
      # create vector for full data indices
      colFull <- numeric(length(unlist(modules[i])))

      # get column names
      colSmall <- colnames(smallData)[unlist(modules[i])]

      # get column indices in full data; append to vector
      for (j in 1:length(colSmall)) {
        colFull[j] <- grep(colSmall[j], colnames(fullData))
      }

      # add list of indices to return vector
      returnMods[i] <- list(colFull)
    }

    return(returnMods)
  }

  # tracking variables
  masterCluster <- data.frame(colName = colnames(fullData), cluster = 1)
  numModules    <- 0

  # use genie (fast agglomerative hierarchical clustering) to cluster data into size 3,000 chunks
  ## transpose data since genie clusters on rows
  clust                 <- genie(t(fullData), k = ceiling(ncol(fullData)/3000), gini_threshold = 0.05)
  masterCluster$cluster <- clust
  clustSize             <- table(clust)

  # Function to continually reduce clusters until largest cluster has less than 4,000 features
  # cs - clustSize, numeric vector of cluster sizes
  # fd - fullData, sample x feature
  reduceClust <- function(cs, fd) {
    # bookkeeping
    largestClust <- max(masterCluster$cluster)
    unique_vals  <- unique(masterCluster$clust)
    clusters     <- masterCluster$cluster

    # keep looping until all clusters are under max cluster size
    while(max(cs) > clusterSize) {
      for (l in 1:length(cs)) {
        # if cluster size is over max cluster size...
        if(cs[l] > clusterSize) {
          # cluster again
          newClust <- genie(t(fd[, which(clusters == unique_vals[l])]),
                            k = ceiling(cs[l]/clusterSize),
                            gini_threshold = 0.05)

          # if genie doesn't do anything, use kmeans
          if(length(unique(newClust)) == 1 | min(table(newClust)) < 50) {
            newClust <- kmeans(t(fd[, which(clusters == unique_vals[l])]),
                               centers = ceiling(cs[l]/clusterSize))$cluster
          }

          # reassign cluster numbers; use <<- to rewrite data from dataframe defined outside function
          masterCluster$cluster[which(clusters == unique_vals[l])] <<- newClust + largestClust

          # update largest cluster number
          largestClust <- max(masterCluster$cluster)
        }
      }

      # update bookkeeping variables
      cs           <- table(masterCluster$cluster)
      unique_vals  <- sort(unique(masterCluster$clust))
      clusters     <- masterCluster$cluster
    }
  }

  reduceClust(cs = clustSize, fd = fullData)
  message(paste0(length(unique(masterCluster$cluster)), " super clusters identified. Beginning Partition."))
  message(paste0("Maximum cluster size: ", max(table(masterCluster$cluster))))

  # set up progress bar
  n_iter <- length(unique(masterCluster$cluster))
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
  part_master <- partition(fullData[, which(masterCluster$cluster == unique(masterCluster$cluster)[1])],
                           threshold, partitioner, tolerance, niter, x, .sep)

  # update indices for each module
  modRows <- grep("reduced_var_", part_master$mapping_key$variable)
  suppressWarnings(part_master$mapping_key$indices <- fullDataColNumbers(fullData = fullData,
                                                                         smallData = fullData[, which(masterCluster$cluster == unique(masterCluster$cluster)[1])],
                                                                         modules = part_master$mapping_key$indices))

  # update number of modules
  numModules  <- numModules + length(grep("reduced_var_", part_master$mapping_key$variable))

  # create superPartition column in mapping key
  part_master$mapping_key$superPartition <- 1

  # progress bar updates
  pb$tick()

  # for each cluster...
  for (i in 2:n_iter) {

    # what to do if cluster is of size one
    if(sum(masterCluster$cluster == unique(masterCluster$cluster)[i]) == 1) {
      # cbind data to master partition reduced data
      part_master$reduced_data <- cbind(part_master$reduced_data,
                                        fullData[, which(masterCluster$cluster == unique(masterCluster$cluster)[i])])

      # rbind data to master partition mapping key
      name <- masterCluster$colName[which(masterCluster$cluster == unique(masterCluster$cluster)[i])]
      part_master$mapping_key <- rbind(part_master$mapping_key,
                                       c(name, name, 1, grep(name, colnames(fullData))))

      # end iteration
      pb$tick()
      break()
    }

    ## partition
    part_clust <- partition(fullData[, which(masterCluster$cluster == unique(masterCluster$cluster)[i])],
                            threshold, partitioner, tolerance, niter, x, .sep)

    ## reduced data
    # get column indices of reduced vars
    modCols <- grep("reduced_var_", colnames(part_clust$reduced_data))

    # update module numbers
    for (j in 1:length(modCols)) {
      colnames(part_clust$reduced_data)[modCols[j]] <- paste0("reduced_var_", numModules+j)
    }

    # cbind to master partition
    part_master$reduced_data <- cbind(part_master$reduced_data, part_clust$reduced_data)

    ## mapping key
    # get row indices of reduced vars
    modRows <- grep("reduced_var_", part_clust$mapping_key$variable)

    # update module number
    for (k in 1:length(modRows)) {
      part_clust$mapping_key$variable[modRows[k]] <- paste0("reduced_var_", numModules+k)
    }

    # update indices
    suppressWarnings(part_clust$mapping_key$indices <- fullDataColNumbers(fullData = fullData,
                                                                          smallData = fullData[, which(masterCluster$cluster == unique(masterCluster$cluster)[i])],
                                                                          modules = part_clust$mapping_key$indices))

    # add superPartition column
    part_clust$mapping_key$superPartition <- i

    # rbind to master partition
    part_master$mapping_key <- rbind(part_master$mapping_key, part_clust$mapping_key)

    # add new modules to overall count
    numModules <- numModules + length(modCols)

    # progress bar updates
    pb$tick()
  }
  return(part_master)
}
