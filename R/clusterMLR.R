#' Cluster Assignment Method
#'
#' @description A function to predict a case's cluster using multinomial logistic regression.
#'
#' @family clusterAssignment
#'
#' @param formula an object of class \code{\link[stats]{formula}}. The details of model specification are given under ‘Details’.
#' @param data a data frame containing the variables in the model. If geographic location is included into the model, name the column to "lat" (or "latitude") as reference to the latitude and "long" (or "longitude") as reference to the longitude.
#' @param newdata a data frame containing the variables of some cases in which we want to predict their clusters.
#' @param threshold optional value to determine the cut-off probability, see "Details".
#' @param nbest how many predicted clusters to choose. Default is 3.
#'
#' @importFrom nnet multinom
#' @import foreach
#' @import dplyr

#' @details
#' \code{formula} argument is in the format of \code{response ~ predictors}.
#' The "response" is a character/factor vector of the clusters' names or indexes. \cr \cr
#' If \code{threshold} is not provided, by default the method will choose the \code{nbest}
#' predicted clusters.
#'
#'
#' @return By default, it returns a data frame showing the \code{nbest} clusters for the first six cases.
#' Some value can be extracted, such as
#' \itemize{
#'    \item{\code{$best_cluster}:}{ a data frame showing the best predicted clusters given \code{threshold} and/or \code{nbest} for every case.
#'    See \code{?getbest} for more details on its data frame.}
#'    \item{\code{$cluster_score}:}{ a data frame showing the predicted probabilities or scores of a case to be assigned into each cluster.
#'    See \code{?getclusterScore} for more details on its data frame.}
#'    \item{\code{$model_fit}}:{ summary of the model fit using multinomial logistic regression.}
#' }
#'
#' @export
#'
clusterMLR <- function(formula, data, newdata, threshold = NULL, nbest = 3){
  if(!is.null(threshold)) msg <- paste("Cluster scores <", threshold, "is discarded.")
  else msg <- paste("Choosing the best", nbest, "clusters.")

  fit <- nnet::multinom(formula, data)
  pred <- predict(fit, newdata = newdata, "probs")
  pred <- as.data.frame(pred)

  myrank <- foreach(i = 1:nrow(pred), .combine = "rbind") %:%
    foreach(j = 1:nbest, .combine = "c") %do% {
      prob.i <- unlist(pred[i,])

      if(!is.null(threshold)) prob.i <- prob.i[which(prob.i >= threshold)]

      rank.i <- match(prob.i, sort(prob.i, T))
      rank.i <- names(prob.i)[which(rank.i == j)]
      if(length(rank.i) == 0) rank.i <- NA
      if(length(rank.i) > 1) rank.i <- paste(rank.i, collapse = ",")
      t(rank.i)
    }
  myrank <- data.frame(myrank, row.names = NULL)
  names(myrank) <- paste("rank", 1:nbest, sep = "_")
  #

  #row.names(pred) <- NULL

  x <- list(method = "multinomial logistic regression",
            message = msg,
            model_fit = fit,
            best_cluster = myrank,
            cluster_score = round(pred, 4))
  new_clusterAssignment(x)
}
