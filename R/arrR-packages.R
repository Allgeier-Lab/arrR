#' @title arrR
#'
#' @description
#' Individual-based simulation model to analyze how artificial reefs in combination
#' with a fish population influence seagrass growth.
#'
#' For more information about how to use the model, please see \url{https://allgeier-lab.github.io/arrR/}.
#'
#' For a detailed model description, see Esquivel, K.E., Hesselbarth, M.H.K., Allgeier, J.E.
#' Mechanistic support for increased primary production around artificial reefs. Manuscript
#' submitted for publication.
#'
#' @name arrR
#' @docType package
#' @useDynLib arrR, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @keywords internal
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
