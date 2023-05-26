#' Extract the Best Cluster/Cases
#'
#' A function to extract a table of the best probable clusters or
#' the predicted cases, ranked by their priorities.
#'
#' @export
getbest <- function(obj){
  UseMethod("getbest")
}


#' @rdname getbest
#' @aliases getbest.clusterAssignment
#' @param obj an object of class "\code{clusterAssignment}" or "\code{seqnextCases}".
#'
#' @return A data frame of the best cluster(s) assignment or the cases which should be sequenced next,
#' depending on the \code{obj}'s class.
#' \itemize{
#'    \item{\code{rank_n}:}{ a column showing the n-th best cluster of a case, ranked by its cluster scores.}
#'    \item{\code{cluster}:}{ cluster's name or index.}
#'    \item{\code{priority_n}:}{ a column showing the n-th best case most likely belongs to a cluster.}
#' }
#' @export
getbest.clusterAssignment <- function(obj){
  if(!inherits(obj, "clusterAssignment")) stop("obj is not of class 'clusterAssignment'.")

  obj$best_cluster
}


#' @rdname getbest
#' @aliases getbest.seqnextCases
#' @export
getbest.seqnextCases <- function(obj){
  if(!inherits(obj, "seqnextCases")) stop("obj is not of class 'seqnextCases'.")

  obj$best_cases
}
