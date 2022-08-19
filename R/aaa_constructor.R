#' Constructor for clusterAssignment class
new_clusterAssignment <- function(x){
  structure(x, class = "clusterAssignment")
}

#' @export
print.clusterAssignment <- function(x){
  cat("\n -- Cluster assignment using", x$method, "-- \n")
  cat("================================================================")
  cat("\n \n")

  msg <- x$message
  cat(msg, "\n")
  cat("Showing only the first six cases.")
  cat("\n \n")
  print(head(x$best_cluster))
  cat("\n")
}



#' Constructor to define plr class
new_plr <- function(x, class = character()){
  structure(x, class = c("plr", class))
}






#' Constructor to define seqnextCases class
new_seqnextCases <- function(x){
  structure(x, class = "seqnextCases")
}

#' @export
print.seqnextCases <- function(x){
  cat("\n -- Predict cases to be sequenced next given clusters -- \n")
  cat("=============================================================")
  cat("\n \n")

  msg <- x$message
  cat(msg, "\n")
  cat("Showing only few cases.")
  cat("\n \n")
  print(head(x$best_cases))
  cat("\n")
}




#' Constructor to define optimumThreshold class
new_optimumThreshold <- function(x){
  structure(x, class = "optimumThreshold")
}

#' @export
print.optimumThreshold <- function(x){
  cat("\n -- The optimum threshold using generalized Youden's index -- \n")
  cat("==================================================================")
  cat("\n \n")

  print(unlist(x[1:5]))
  cat("\n")
}

#' @export
plot.optimumThreshold <- function(x, ...){
  theroc <- x$roc
  plot.roc(theroc, thresholds="best",
           print.thres="best",
           print.thres.best.weights=c(x$cost_ratio, x$pair_proportion), ...)
}
