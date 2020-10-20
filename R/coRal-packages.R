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
  "density",
  "detritus_dead",
  "detritus_pool",
  "died_background",
  "died_consumption",
  "i",
  "layer",
  "max_i",
  "nutrients_pool",
  "reef",
  "save_each",
  "summary_function",
  "timestep",
  "value",
  "weight",
  "x",
  "y"))
