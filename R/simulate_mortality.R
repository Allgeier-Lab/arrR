#' simulate_mortality
#'
#' @description Simulate background mortality of population.
#'
#' @param fishpop_values,fishpop_track Data frame population created
#' with \code{\link{setup_fishpop}}.
#' @param n_pop Numeric with number of individuals.
#' @param seafloor,seafloor_values RasterBrick and matrix with seafloor values.
#' @param parameters List with all model parameters.
#' @param min_per_i Integer to specify minutes per i.
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
simulate_mortality <- function(fishpop_values, fishpop_track,
                               seafloor, seafloor_values,
                               n_pop, parameters, min_per_i) {

  # create death probability
  death_prob <- exp(fishpop_values[, "length"] - parameters$pop_max_size)

  # create random number to test death prob against
  random_prob <- stats::runif(n = n_pop, min = 0, max = 1)

  # identify who dies
  fish_id <- which(random_prob < death_prob)

  # check if mortality occurs
  if (length(fish_id) > 0) {

    # randomize order of loop because detritus pool can "run out"
    fish_id <- sample(fish_id, size = length(fish_id))

    # get detritus/nutrient pools at location and raster cells
    cell_id <- raster::cellFromXY(object = seafloor,
                                  xy = fishpop_values[fish_id, c("x", "y"),
                                                      drop = FALSE])

    # create new individual
    rcpp_create_rebirth(fishpop = fishpop_values,
                        fishpop_track = fishpop_track,
                        seafloor = seafloor_values,
                        fish_id = fish_id,
                        cell_id = cell_id,
                        pop_n_body = parameters$pop_n_body,
                        pop_want_reserves = parameters$pop_want_reserves)

  }
}
