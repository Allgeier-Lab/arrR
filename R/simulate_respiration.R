#' simulate_respiration
#'
#' @description Simulate respiration of population.
#'
#' @param fishpop_values Matrix with population created with \code{\link{setup_fishpop}}.
#' @param parameters List with all model parameters.
#' @param water_temp Numeric with water temperature.
#' @param min_per_i Integer to specify minutes per i.
#'
#' @details
#' Function to simulate respiration of fish population individuals.
#'
#' @references
#' Hanson, P.C., Johnson, T.B., Schindler, D.E., Kitchell, J.F., 1997. Fish
#' Bioenergetics 3.0 for Windows manual (Manual). University of Wisconsin-Madison,
#' Centre for Limnology, Madison,USA.
#'
#' @return matrix
#'
#' @aliases simulate_respiration
#' @rdname simulate_respiration
#'
#' @export
simulate_respiration <- function(fishpop_values, parameters, water_temp, min_per_i) {

  rcpp_respiration(fishpop = fishpop_values,
                   resp_intercept = parameters$resp_intercept,
                   resp_slope = parameters$resp_slope,
                   resp_temp_low = parameters$resp_temp_low,
                   resp_temp_optm = parameters$resp_temp_optm,
                   resp_temp_max = parameters$resp_temp_max,
                   water_temp = water_temp, min_per_i = min_per_i)

}
