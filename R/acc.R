#' A Function to Compute the Accuracy of the Prediction
#'
#' A function to compute accuracy of the prediction.
#'
#' @export
acc <- function(obj, true.cluster){
  UseMethod("acc")
}

#' @rdname acc
#' @aliases acc.clusterAssignment
#' @param obj an object of class "\code{clusterAssignment}".
#' @param true.cluster the true cluster of the new cases.
#'
#' @details If the \code{obj} is of class "\code{clusterAssignment}", the accuracy is
#' the fraction of true assignments within the predicted plausible clusters. For example,
#' if \code{nbest = 3} and the true cluster of a case is one of those 3 predicted clusters, then
#' the method assigns the case correctly. \cr \cr
#' If the \code{obj} is of class "\code{seqnextCases}", the accuracy is computed as the
#' fraction of cases which correctly are assigned to their true clusters. The accuracy of a cluster is
#' returned as \code{NaN} if there is no cases in such a cluster in the training set, but there is at
#' least one cases of such a cluster in the test set.
#'
#'
#' @return A numeric or data frame depicting the accuracy of the prediction.
#' If the function is returned as data frame, it contains the following colums:
#' \itemize{
#'    \item{\code{cluster}:}{ a vector of the clusters' names in the test set.}
#'    \item{\code{accuracy}:}{ the fraction of correct assignment given cluster.}
#' }
#'
#' @export
acc.clusterAssignment <- function(obj, true.cluster){
  if(!inherits(obj, "clusterAssignment")) stop("obj is not of class 'clusterAssignment'.")


  assgn <- bind_cols(data.frame(cluster = true.cluster), obj$best_cluster)
  is.correct <- foreach(i = 1:nrow(assgn), .combine = "c") %do% {
    rank.i <- select(assgn, -c("cluster"))[i,]
    rank.i <- unlist(sapply(rank.i, function(x) strsplit(x, ","), simplify = T), use.names = F)

    if(!all(is.na(rank.i))) rank.i <- rank.i[!is.na(rank.i)]
    if(assgn$cluster[i] %in% rank.i) 1 else 0
  }


  sum(is.correct == 1)/length(is.correct)

}


#' @rdname acc
#' @aliases acc.seqnextCases
#' @export
acc.seqnextCases <- function(obj, true.cluster){
  if(!inherits(obj, "seqnextCases")) stop("obj is not of class 'seqnextCases'.")
  if(length(true.cluster) != length(obj$id)) stop("true.cluster has different length with the case id in obj.")



  # compute the acc
  nextcases <- obj$best_cases
  true.clusterdf <- data.frame(id = obj$id, cluster = true.cluster)
  correctdf <- foreach(i = 1:nrow(nextcases), .combine = "rbind") %do% {
    cases.i <- select(nextcases, -c("cluster"))[i,]
    cases.i <- unlist(sapply(cases.i, function(x) strsplit(x, ","), simplify = T), use.names = F)

    if(!all(is.na(cases.i))) cases.i <- cases.i[!is.na(cases.i)]
    tr.cluster.i <- with(true.clusterdf, cluster[which(id %in% cases.i)])
    m <- sum(ifelse(nextcases$cluster[i] == tr.cluster.i, 1, 0)) #of cases that are correctly classified
    nc <- length(cases.i) #of selected cases. May be > #of priority chosen if there are ties.
    data.frame(m = m, nc = nc)
  }
  correctdf <- data.frame(cluster = nextcases$cluster) %>% bind_cols(correctdf) %>%
    left_join(as.data.frame(table(true.cluster)), by = c("cluster"="true.cluster")) %>%
    mutate(N = ifelse(nc <= (ncol(nextcases)-1), pmin(nc, Freq), nc))




  # if there is no ties, #of chosen cases is the #of nbest priority. But, if there are ties, #of chosen cases is at least the #of nbest priority.
  # thus, acc = <#of correct cases> / <min(#of chosen cases, #of cluster true size)> if no ties,
  # or acc = <#of correct cases> / <#of chosen cases> if ties.
  # The reason why we use <min(#of chosen cases, #of cluster true size)>, is bc if #of cluster true size < #of chosen cases, we get incorrect
  # assignment due to forcing the algorithm to sample K best cases to be sequenced.
  acc <- with(correctdf, m/N)
  names(acc) <- correctdf$cluster

  data.frame(cluster = names(acc), accuracy = acc, row.names = NULL)
}
