#' Fit Pairwise Logistic Regression
#'
#' @description A function to fit a pairwise logistic regression.
#'
#' @param formula an object of class \code{\link[stats]{formula}}. The details of model specification are given under "Details".
#' @param data a data frame containing the variables in the model. If geographic location is included into the model, name the
#' column to "lat" (or "latitude") as reference to the latitude and "long" (or "longitude") as reference to the longitude.
#'
#'
#' @details \code{formula} argument is in the format of \code{response ~ predictors}.
#' The "response" is a character/factor vector of the clusters' names or indexes.
#'
#' @return Returns an object of class "\code{plr}", which inherits from "glm" class and "lm" class.
#'
#' @examples
#'
#' @export
plr <- function(formula, data){
  data <- as.data.frame(data)
  # Transform to pairwise data
  #mod <- formula(formula)
  #split.mod <-  unlist(stringr::str_split(mod, "\ ~"))
  mod <- as.character(formula)
  yname <- split.mod[2]

  if(split.mod[3] == ".") varname <- names(data)[names(data) != yname]
  else varname <- unlist(stringr::str_split(split.mod[3], "\ +"))

  varnameLoc <- varname[varname %in% c("lat", "long", "latitude", "longitude", "Lat", "Long", "Latitude", "Longitude")]
  varnameX <- varname[!(varname %in% c("+", varnameLoc))]

  if(length(varnameX) == 0){
    X <- NULL
  } else if(length(varnameX) == 1){
    X <- as.data.frame(data[, varnameX])
    names(X) <- varnameX
  } else{
    X <- data[, varnameX]
  }
  location <- data[, varnameLoc]
  #



  # create pairwise data for training set
  if(length(location) > 0){
    predictor <- paste(c(varnameX, "Spatial"), collapse = "+")
    traindata <- zCovariate(data[,yname], X, location, F)
  } else{
    predictor <- paste(varnameX, collapse = "+")
    traindata <- zCovariate(data[,yname], X, .removeRepetition = F)
  }
  #



  # fit pairwise LR
  mod <- formula(paste("y", predictor, sep = "~"))
  myfit <- glm(mod, family = binomial(), data = traindata)



  new_plr(myfit, class(myfit))
}
