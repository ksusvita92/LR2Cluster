#' Estimate the Cluster Size Increment
#'
#' A function to estimate the cluster size increment after assigning unsequenced cases.
#'
#' @export
clusterSize <- function(obj, rho = 0){
  UseMethod("clusterSize")
}

#' @rdname clusterSize
#' @aliases clusterSize.clusterAssignment
#' @param obj an object of class "\code{clusterAssignment}".
#' @param rho a probability that a case does not belong to any of the given clusters.
#'
#' @return A data frame of the estimated increment of clusters.
#'
#' @export
clusterSize.clusterAssignment <- function(obj, rho = 0){
  if(!inherits(obj, "clusterAssignment")) stop("obj is not of class 'clusterAssignment'.")


  # get q
  myscore <- obj$cluster_score
  score.rowsum <- apply(myscore, 1, function(x) sum(x))
  myq <- myscore/score.rowsum



  # cluster size
  c_size <- (1-rho)*colSums(myq)

  data.frame(cluster = names(c_size), increment = c_size, row.names = NULL)
}
