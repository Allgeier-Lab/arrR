#' simulate_growth
#'
#' @description Simulate consumption of fish population.
#'
#' @param fishpop_values,fishpop_track Matrix with fish population.
#' @param seafloor_values RasterBrick and matrix with seafloor values.
#' @param parameters List with all model parameters.
#' @param extent,dimensions Spatial extent and dimensions of the seafloor raster
#' @param min_per_i Integer to specify minutes per i.
#'
#' @details
#' Function to simulate consumption, possible mortality, growth and excretion of fish
#' population.
#'
#' @return list
#'
#' @aliases simulate_growth
#' @rdname simulate_growth
#'
#' @export
simulate_growth <- function(fishpop_values, fishpop_track, seafloor_values,
                            parameters, extent, dimensions, min_per_i) {

  rcpp_fishpop_growth(fishpop = fishpop_values, fishpop_track = fishpop_track,
                      seafloor = seafloor_values,
                      pop_k = parameters$pop_k, pop_linf = parameters$pop_linf,
                      pop_a = parameters$pop_a, pop_b = parameters$pop_b,
                      pop_n_body = parameters$pop_n_body,
                      pop_want_reserves = parameters$pop_want_reserves,
                      pop_max_reserves = parameters$pop_max_reserves,
                      pop_consumption_prop = parameters$pop_consumption_prop,
                      extent = as.vector(extent, mode = "numeric"), dimensions = dimensions,
                      min_per_i = min_per_i)

}
