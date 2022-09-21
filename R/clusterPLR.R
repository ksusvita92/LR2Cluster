#' Cluster Assignment Method
#'
#' @description A function to predict a case's cluster using pairwise logistic regression.
#'
#' @family clusterAssignment
#'
#' @param formula an object of class \code{\link[stats]{formula}}. The details of model specification are given under ‘Details’.
#' @param data a data frame containing the variables in the model. If geographic location is included into the model, name the column to "lat" (or "latitude") as reference to the latitude and "long" (or "longitude") as reference to the longitude.
#' @param newdata a data frame containing the variables of some cases in which we want to predict their clusters.
#' @param threshold optional value to determine the cut-off probability, see "Details".
#' @param nbest how many predicted clusters to choose. Default is 3.
#'
#' @import foreach
#' @import dplyr
#'
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
clusterPLR <- function(formula, data, newdata, threshold = NULL, nbest = 3){
  if(!is.null(threshold)) msg <- paste("Cluster scores <", threshold, "is discarded.")
  else msg <- paste("Choosing the best", nbest, "clusters.")

  data <- as.data.frame(data)
  newdata <- as.data.frame(newdata)

  mod <- formula(formula)
  split.mod <-  unlist(stringr::str_split(mod, "\ ~"))
  yname <- split.mod[2]

  if(split.mod[3] == ".") varname <- names(data)[names(data) != yname]
  else varname <- unlist(stringr::str_split(split.mod[3], "\ +"))

  varnameLoc <- varname[varname %in% c("lat", "long", "latitude", "longitude", "Lat", "Long", "Latitude", "Longitude")]
  varnameX <- varname[!(varname %in% c("+", varnameLoc))]

  location <- data[, varnameLoc]
  newlocation <- newdata[, varnameLoc]

  if(length(varnameX) == 0){
    X <- NULL
    X0 <- NULL
  } else if(length(varnameX) == 1){
    X <- as.data.frame(data[, varnameX])
    names(X) <- varnameX
    X0 <- as.data.frame(newdata[, varnameX])
    names(X0) <- varnameX
  } else{
    X <- data[, varnameX]
    X0 <- newdata[, varnameX]
  }



  # create pairwise data for test set
  if(nrow(X0) > 0 && !is.null(X0)){
    Z0  <- foreach(i = 1:nrow(X0), .combine = "rbind") %:%
      foreach(j = 1:ncol(X0), .combine = "cbind") %do% {
        x <- X[,j]
        x0 <- X0[i,j]
        if(class(x0) == "factor" || class(x0) == "character") res <- ifelse(x0 == x, x0, "DIFF")
        else{
          res <- abs(x0-x)
          res <- as.numeric(res)
        }
        as.data.frame(res)
      }
    names(Z0) <- names(X0)
  } else Z0 <- NULL


  if(length(location) > 0 && length(newlocation) > 0){
    # update lat, long in the formula to Spatial
    predictor <- paste(c(varnameX, "Spatial"), collapse = "+")

    spatial <- foreach(i = 1:nrow(newlocation), .combine = "rbind") %do% geodist::geodist(x = location, y = newlocation[i,], measure="haversine")/1000
    Z0 <- Z0 %>% bind_cols(data.frame(Spatial = spatial))
  } else predictor <- paste(varnameX, collapse = "+")
  #



  # create pairwise data for training set
  if(length(location) > 0) traindata <- zCovariate(data[,yname], X, location, F)
  else traindata <- zCovariate(data[,yname], X, .removeRepetition = F)



  # get the cluster score
  mod <- formula(paste("y", predictor, sep = "~"))
  myfit <- glm(mod, family = binomial(), data = traindata)
  mypred <- predict(myfit, Z0, type = "resp")

  id <- paste("cs", 1:nrow(newdata), sep = "_")
  id <- factor(id, levels = id)
  myscore <- data.frame(id = rep(id, each = nrow(data)), cluster = rep(data[,yname], nrow(newdata)), pred = mypred) %>%
    group_by(id, cluster) %>% summarise(prob = max(pred)) %>% ungroup() %>% tidyr::spread(., cluster, prob) %>% as.data.frame()
  myscore <- myscore[, -1]
  #


  # get the best clusters
  myrank <- foreach(i = 1:nrow(myscore), .combine = "rbind") %:%
    foreach(j = 1:nbest, .combine = "c") %do% {
      score.i <- unlist(myscore[i,1:ncol(myscore)])

      if(!is.null(threshold)) score.i <- score.i[which(score.i >= threshold)]

      rank.i <- match(score.i, sort(score.i, T))
      rank.i <- names(score.i)[which(rank.i == j)]
      if(length(rank.i) == 0) rank.i <- NA
      if(length(rank.i) > 1) rank.i <- paste(rank.i, collapse = ",")
      t(rank.i)
    }

  myrank <- data.frame(myrank, row.names = NULL)
  names(myrank) <- paste("rank", 1:nbest, sep = "_")
  #


  x <- list(method = "pairwise logistic regression",
            message = msg,
            model_fit = summary(myfit),
            best_cluster = myrank,
            cluster_score = round(myscore,4))
  new_clusterAssignment(x)
}
