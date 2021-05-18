#' simulate_mortality
#'
#' @description Simulate background mortality of population.
#'
#' @param fishpop_values,fishpop_track Matrix with fish population.
#' @param pop_n Numeric with number of individuals.
#' @param seafloor_values RasterBrick and matrix with seafloor values.
#' @param parameters List with all model parameters.
#' @param extent,dimensions Spatial extent and dimensions of the seafloor raster
#'
#' @details
#' Function to simulate background mortality of fish population individuals.
#'
#' @return list
#'
#' @aliases simulate_mortality
#' @rdname simulate_mortality
#'
#' @export
simulate_mortality <- function(fishpop_values, fishpop_track, pop_n, seafloor_values,
                               parameters, extent, dimensions) {

  # randomize order of loop because detritus pool can "run out"
  fish_id <- sample(x = seq(from = 1, to = pop_n), size = pop_n)

  # create new individual
  rcpp_mortality_backgr(fishpop = fishpop_values, fishpop_track = fishpop_track,
                        fish_id = fish_id, seafloor = seafloor_values,
                        pop_linf = parameters$pop_linf, pop_n_body = parameters$pop_n_body,
                        pop_want_reserves = parameters$pop_want_reserves,
                        extent = as.vector(extent, mode = "numeric"), dimensions = dimensions)

}
