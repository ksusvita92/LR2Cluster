#' Get Cluster Score
#'
#' A function to extract the cluster scores of unsequenced cases in each cluster.
#'
#' @export
getclusterScore <- function(obj){
  UseMethod("getclusterScore")
}
#'
#' @rdname getclusterScore
#' @aliases getclusterScore.clusterAssignment

#' @param obj an object of class "\code{clusterAssignment}".
#'
#' @return A data frame of the new cases' cluster scores.
#'
#'
#' @export
getclusterScore.clusterAssignment <- function(obj){
  if(!inherits(obj, "clusterAssignment")) stop("obj is not of class 'clusterAssignment'.")

  obj$cluster_score
}
