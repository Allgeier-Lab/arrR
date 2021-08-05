#' @title arrR
#'
#' @description
#' Individual-based simulation model to analyze how artificial reefs in combination
#' with a fish population influence seagrass growth.
#'
#' For more information about how to use the model, please see \code{browseVignettes("arrR")}.
#'
#' For a detailed model description, see Esquivel, K., Hesselbarth, M.H.K., Allgeier, J.E.
#' In preparation. Mechanistic support for increased primary production around artificial reefs.
#'
#' @name arrR
#' @docType package
#' @useDynLib arrR, .registration = TRUE
#' @importFrom Rcpp sourceCpp
"_PACKAGE"

globalVariables(names = c(
  "ag_biomass",
  "bg_biomass",
  "burn_in",
  "density",
  "detritus_fish",
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
  "thres",
  "timestep",
  "value",
  "weight",
  "x",
  "y"))
