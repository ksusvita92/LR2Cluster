#' Get the Optimum Threshold
#'
#' A function to obtain the optimum threshold given a cost ratio using \emph{Generalized Youden's Index}. See "Reference"
#'
#' @param response a (factor, numeric, or character) vector of responses (true class). Typically encoded with 0 (if a pair of cases are in the different clusters)
#' and 1 (if a pair of cases are in the same cluster).
#' @param prediction a (numeric) vector of the same length as \code{response}. It contains the predicted probability if a pair of new cases are in the same cluster.
#' @param cost.ratio the relative cost of a false negative classification as compared to a false positive classification.
#'
#' @import pROC
#'
#' @return By default, it returns a table showing the optimum threshold and some true positive (and negative) rates.
#' Some value can be extracted, such as
#' \itemize{
#'    \item{\code{$threshold}:}{ the optimum threshold given cost ratio.}
#'    \item{\code{$specificity}:}{ the true negative rate evaluated at the optimum threshold.}
#'    \item{\code{$sensitivity}:}{ the true positive rate evaluated at the optimum threshold.}
#'    \item{\code{$accuracy}:}{ a fraction of correctly predicting the response variable.}
#'    \item{\code{$pair_proportion}:}{ a fraction of pairs who are in the same cluster.}
#'    \item{\code{$roc}:}{ an object of class "roc". See \code{?pROC::roc}.}
#' }
#'
#' @references
#' \itemize{
#'    \item{Greiner M, Pfeiffer D, Smith RD. Principles and practical application of the receiver-operating characteristic analysis for diagnostic tests. Prev Vet Med. 2000 May 30;45(1-2):23-41. doi: 10.1016/s0167-5877(00)00115-x. PMID: 10802332.}
#'    \item{Schisterman EF, Perkins NJ, Liu A, Bondell H. Optimal cut-point and its corresponding Youden Index to discriminate individuals using pooled blood samples. Epidemiology. 2005 Jan;16(1):73-81. doi: 10.1097/01.ede.0000147512.81966.ba. PMID: 15613948.}
#' }
#'
#' @examples
#'
#' @export
optThreshold <- function(response, prediction, cost.ratio = 1){
  prev <- length(which(response == 1))/length(response)

  roc <- roc(response ~ prediction, direction = "<")
  mycoords <- coords(roc = roc,
                     x = "best",
                     input = "threshold",
                     best.method = "youden",
                     best.weights = c(cost.ratio, prev),
                     transpose = F)

  # get accuracy
  acc <- ifelse(prediction < mycoords$threshold, 0, 1)
  acc <- sum(acc == response)/length(response)

  x <- list(threshold = mycoords$threshold,
            specificity = mycoords$specificity,
            sensitivity = mycoords$sensitivity,
            accuracy = acc,
            auc = roc$auc,
            pair_proportion = prev,
            cost_ratio = cost.ratio,
            roc = roc)

  new_optimumThreshold(x)
}



