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
  "died_background",
  "died_consumption",
  "layer",
  "nutrients_pool",
  "reef",
  "summary_function",
  "timestep",
  "weight",
  "x",
  "y"))
