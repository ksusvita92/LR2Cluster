#' \emph{M. Tuberculosis} Cases in Valencia, Spain
#'
#' A data frame containing the genomic clusters and metadata of some TB cases in Valencia, Spain between 2014 and 2016.
#'
#' @format A data frame with 531 rows and 11 variables:
#' \describe{
#'   \item{\code{id_server}}{a character vector of case id's.}
#'   \item{\code{tr_cl}}{genomic clusters. Two cases are in the same cluster is the genomic distance is < 15 SNPs. \code{unique} means that the case is unclustered.}
#'   \item{\code{age}}{case's age in years.}
#'   \item{\code{sex}}{genders, labelled as: \code{FEMALE} or \code{MALE}.}
#'   \item{\code{foreign}}{a case's nationality, labelled as: "\code{YES}" if foreign-born and "\code{No}" if Spanish-born."}
#'   \item{\code{dx_date}}{diagnosis date.}
#'   \item{\code{location}}{TB location.}
#'   \item{\code{diabetes}}{a factor indicating if a case has diabetes Mellitus.}
#'   \item{\code{hiv}}{a factor indicating if a case has HIV}
#'   \item{\code{latitude, longitude}}{a case's residential location.}
#' }
#' @source Xu Y, Cancino-Muñoz I, Torres-Puente M, Villamayor LM, Borrás R, et al. (2019) High-resolution mapping of tuberculosis transmission:
#' Whole genome sequencing and phylogenetic modelling of a cohort from Valencia Region, Spain. PLOS Medicine 16(10): e1002961.
#' \url{https://doi.org/10.1371/journal.pmed.1002961}
"tb_valencia"
