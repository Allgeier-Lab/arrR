#' @title coRal
#'
#' @description
#' More about what it does (maybe more than one line)
#'
#' @name coRal
#' @docType package
#' @useDynLib coRal, .registration = TRUE
#' @importFrom Rcpp sourceCpp
"_PACKAGE"

globalVariables(names = c(
  "ag_biomass",
  "bg_biomass",
  "detritus_dead",
  "detritus_pool",
  "died_consumption",
  "died_background",
  "i",
  "id",
  "timestep",
  "wc_nutrients",
  "weight",
  "x",
  "y"))
