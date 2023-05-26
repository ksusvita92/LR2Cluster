#' Convert dist to data.frame
#'
#'
distToDF <- function(inDist, val.name) {
  if (class(inDist) != "dist") stop("wrong input type")
  A <- attr(inDist, "Size")
  B <- if (is.null(attr(inDist, "Labels"))) sequence(A) else attr(inDist, "Labels")
  if (isTRUE(attr(inDist, "Diag"))) attr(inDist, "Diag") <- FALSE
  if (isTRUE(attr(inDist, "Upper"))) attr(inDist, "Upper") <- FALSE
  df = data.frame(
    row = B[unlist(lapply(sequence(A)[-1], function(x) x:A))],
    col = rep(B[-length(B)], (length(B)-1):1),
    value = as.vector(inDist), stringsAsFactors=F)
  names(df) = c("row", "col", val.name)
  return(df)
}

#' Compare character vector
compareDummy <- function(x, val.name) {
  dist <- stringdist::stringdistmatrix(x, method = "osa", useNames = "names")
  df <- distToDF(dist, val.name)
  df[[val.name]] <- ifelse(df[[val.name]] == 0, "SAME", "DIFF")
  id <- which(df[[val.name]] == "SAME")
  nm <- as.character(df$row[id])

  for(lev in unique(x)) {
    lab <- names(x)[x == lev]
    nm_id <- which(nm %in% lab)
    nm[nm_id] <- lev
  }
  df[[val.name]][id] <- nm
  return(df)
}


#' Compare cluster
compareCluster = function(x, val.name) {
  if(any(is.na(x))) stop("There's NA in the data")
  dist <- stringdist::stringdistmatrix(x, method = "osa", useNames = "names")
  df <- distToDF(dist, val.name)
  df[[val.name]] <- ifelse(df[[val.name]] == 0, "SAME", "DIFF")
  id <- which(df[[val.name]] == "SAME")
  nm <- as.character(df$row[id])

  lab <- names(x)[x == "unique"]
  nm_id <- which(nm %in% lab)

  if(length(df[id,][nm_id,][[val.name]]) != 0) {
    df[id,][nm_id,][[val.name]] <- "DIFF"
  }
  return(df)
}


#' Compute geospatial distance
geoSpatial <- function(case.id, loc){
  xx <- loc
  yy <- geodist::geodist(xx, measure="haversine")/1000
  rownames(yy) <- case.id; colnames(yy) <- case.id
  yy <- as.dist(yy)
  return(distToDF(yy, "Spatial"))
}
