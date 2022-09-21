#' Model Prediction
#'
#' @description Obtains predictions and estimates standard errors from a fitted pairwise logistic regression model object.
#'
#' @param obj a fitted object of a class from "\code{plr}".
#' @param newdata a data frame containing the variables of some cases in which we want to predict.
#' @param case.id optional vector of case id or index in the \code{newdata}.
#'
#'
#' @return a data frame containing:
#' \itemize{
#'    \item{\code{y}: }{the predicted probability if \code{case-to.case} is in the same cluster.}
#'    \item{\code{se.y:} }{the standard error of \code{y}.}
#' }
#'
#'
#' @export
predict.plr <- function(obj, newdata, case.id = NULL){
  if(!inherits(obj, "plr")) stop("obj is not of class 'plr'.")
  if(is.null(case.id)) case.id <- paste("cs", 1:nrow(newdata), sep = "_")

  newdata <- as.data.frame(newdata)

  # get pairwise data
  mod <- obj$formula
  split.mod <-  unlist(stringr::str_split(mod, "\ ~"))
  varname <- unlist(stringr::str_split(split.mod[3], "\ +"))
  varnameX <- varname[!(varname %in% c("+", "Spatial"))]
  if("Spatial" %in% varname) varnameLoc <- names(newdata)[names(newdata) %in% c("lat", "long", "latitude", "longitude")]
  else varnameLoc <- character(0)

  newlocation <- newdata[, varnameLoc]
  if(length(varnameX) == 0){
    X0 <- NULL
  } else if(length(varnameX) == 1){
    X0 <- as.data.frame(newdata[, varnameX])
    names(X0) <- varnameX
  } else{
    X0 <- newdata[, varnameX]
  }


  if(length(newlocation) > 0) pairdata <- zCovariate(rep("uncluster", nrow(newdata)), X0, newlocation, F, case.id)
  else pairdata <- zCovariate(rep("uncluster", nrow(newdata)), X0, .removeRepetition = F, id = case.id)



  res <- predict.glm(obj, pairdata, type = "response", se.fit = T)
  data.frame(case = pairdata$case, to.case = pairdata$to.case, y = res$fit, se.y = res$se.fit)
}
