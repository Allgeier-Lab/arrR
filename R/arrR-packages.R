#' @title arrR
#'
#' @description
#' Individual-based simulation model to analyze how artificial reefs in combination
#' with a fish population influence seagrass growth.
#'
#' @name arrR
#' @docType package
#' @useDynLib arrR, .registration = TRUE
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
