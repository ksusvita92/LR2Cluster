#' Transform Individual-level Data to Pairwise-level Data
#'
#' @description Transform individual-level data into pairwise-level data.
#'
#' @param cluster cluster's name or index.
#' @param X a data frame of individual-level data.
#' @param location a matrix or data frame containing latitude and longitude coordinates.
#' @param .removeRepetition a logical value to exclude repetitive rows after transformation. Default is FALSE.
#' @param id a case's name or index, optional.
#'
#' @import stringr
#' @import stringdist
#' @import geodist
#'
#' @return A data frame containing the pairwise-level data.
#' \itemize{
#'    \item{\code{y}:}{ a binary vector indicating if a pair of cases are in the same cluster.
#'    \code{y=1} means a pair are in the same cluster, and \code{y = 0} means otherwise.}
#'    \item{\code{Spatial}:}{ the geodesic distance (in km) between two cases and only appear if \code{location} is not \code{NULL}.}
#' }
#'
#'
#' @export
zCovariate <- function(cluster, X = NULL, location = NULL, .removeRepetition = F, id = NULL){
  if(is.null(id)) id <- paste("c", 1:length(cluster), sep = "_")
  if(class(cluster) == "character") cluster <- factor(cluster)


  #create pairwise response var
  names(cluster) <- id
  cluster <- compareCluster(cluster, val.name="y")
  cluster$y <- ifelse(cluster$y=="SAME",1,0)
  Z <- cluster

  #create spatial
  if(!is.null(location)){
    x <- geoSpatial(id, location)
    Z <- Z %>%
      left_join(x, by = c("row"="row", "col"="col"))
  }

  #create pairwise X
  if(!is.null(X)){
    for(i in 1:ncol(X)){
      if(class(X[[i]]) == "character") X[[i]] <- factor(X[[i]])
      x <- X[[i]]
      names(x) <- id
      if(class(x) == "factor") x <- compareDummy(x, colnames(X)[i])
      else{
        x <- dist(x)
        x <- distToDF(x, colnames(X)[i])
      }
      Z <- Z %>%
        left_join(x, by = c("row"="row", "col"="col"))
    }
  }

  names(Z)[1:2] <- c("case", "to.case")


  if(.removeRepetition) distinct(Z, across(-c(case, to.case)), .keep.all = T) else Z
}
