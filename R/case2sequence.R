#' Choose Which Cases to be Sequenced Next Given Clusters
#'
#' @description A function to predict the most likely new cases to be sequenced next given a cluster. It ranks
#' the cases based on their priorities in each cluster.
#'
#' @param obj an object of class "\code{clusterAssignment}".
#' @param case.id a (optional) vector of case id in which we should choose to be sequenced next.
#' @param threshold optional value to determine the cut-off probability, see "Details". Default is 0.
#' @param nbest how many predicted clusters to choose. Default is 3.
#'
#' @details The function ranks each case and determines which cases have higher priorities to be assigned to a cluster.
#' If \code{threshold} is not provided, the \code{nbest} predicted clusters are chosen. \cr \cr
#' If \code{case.id}} is not provided, the function will generate a vector of id's.
#'
#' @return By default, it shows data frame showing which cases to be sequenced next for a subset of clusters.
#' There are some values that can be extracted such as:
#' \itemize{
#'    \item{\code{$id}:}{ a vector of case id.}
#'    \item{\code{$best_cases}:}{ a data frame which determines which cases to be sequenced next for every cluster.}
#' }
#'
#'
#' @examples
#' @export
case2sequence <- function(obj, case.id = NULL, threshold = NULL, nbest = 3){
  if(!inherits(obj, "clusterAssignment")) stop("obj must be of class 'clusterAssignment'.")

  if(!is.null(threshold)) msg <- paste("Cases with cluster score <", threshold, "is discarded.")
  else msg <- paste("Choosing the best", nbest, "cases to be sequenced next.")

  if(is.null(case.id)) case.id <- paste("cs", 1:nrow(obj$cluster_score), sep = "_")



  # find the priority cases to be sequences
  object <- obj$cluster_score
  mypriority <- foreach(j = 1:ncol(object), .combine = "rbind") %:%
    foreach(i = 1:nbest, .combine = "c") %do% {
      cluster.score <- object[,j]
      names(cluster.score) <- case.id

      if(!is.null(threshold)) cluster.score <- cluster.score[which(cluster.score >= threshold)]

      rank.score <- match(cluster.score, sort(cluster.score, T))
      priority.i <- names(cluster.score)[which(rank.score == i)]

      if(length(priority.i) == 0) priority.i <- NA
      if(length(priority.i) > 1) priority.i <- paste(priority.i, collapse = ",")

      priority.i
    }
  mypriority <- data.frame(mypriority, row.names = NULL)
  names(mypriority) <- paste("priority", 1:nbest, sep = "_")
  mypriority <- data.frame(cluster = colnames(object)) %>% bind_cols(mypriority)
  #




  x <- list(message = msg, id = case.id, best_cases = mypriority)
  new_seqnextCases(x)
}
