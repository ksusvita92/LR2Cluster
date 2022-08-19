#' @title Cluster Assignment Method
#'
#' @description A function to assign cases in clusters randomly, proportional to the cluster's size.
#'
#' @family clusterAssignment
#'
#' @param x a (character or factor) vector of the true clusters in the training data.
#' @param N number of cases in which we want to predict their clusters. Default is 1.
#' @param threshold optional value to determine the cut-off probability, see "Details".
#' @param nbest how many predicted clusters to choose. Default is 3.
#'
#' @import foreach
#' @import dplyr

#' @details If \code{threshold} is not provided, by default the method will choose the \code{nbest}
#' predicted clusters randomly.
#'
#'
#' @return By default, it returns a data frame showing the \code{nbest} clusters for the first six cases.
#' Some value can be extracted, such as
#' \itemize{
#'    \item{\code{$best_cluster}:}{ a data frame showing the best predicted clusters given \code{threshold} and/or \code{nbest} for every case.
#'    See \code{?getbest} for more details on its data frame.}
#'    \item{\code{$cluster_score}:}{ a data frame showing the predicted probabilities or scores of a case to be assigned into each cluster.
#'    See \code{?getclusterScore} for more details on its data frame.}
#' }
#'
#'
#' @export
clusterRandom <- function(x, N = 1, threshold = NULL, nbest = 3){
  if(!is.null(threshold)) msg <- paste("Cluster scores <", threshold, "is discarded.")
  else msg <- paste("Choosing the best", nbest, "clusters.")

  myprob <- table(x)/length(x)

  if(!is.null(threshold)) myprob <- myprob[myprob >= threshold]
  myprob <- data.frame(as.list(myprob))

  myrank <- foreach(i = 1:N, .combine = "rbind") %do% {
    n <- ifelse(nbest > ncol(myprob), ncol(myprob), nbest)
    rank.i <- sample(names(myprob), n, prob = unlist(myprob[1,]))

    if(n < nbest){
      tmp <- rep(NA, nbest)
      tmp[1:n] <- rank.i
      rank.i <- tmp
    }
    rank.i
  }
  myrank <- data.frame(myrank, row.names = NULL)
  names(myrank) <- paste("rank", 1:nbest, sep = "_")
  #



  myscore <- data.frame(as.list(table(x)/length(x))) %>% slice(rep(1:n(), N))

  x <- list(method = "random guess",
            message = msg,
            model_fit = NA,
            best_cluster = myrank,
            cluster_score = round(myscore, 4))
  new_clusterAssignment(x)
}
