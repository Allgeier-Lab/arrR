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
  id_mort <- which(random_prob < death_prob)

  # check if mortality occurs
  if (length(id_mort) > 0) {

    # get detritus/nutrient pools at location and raster cells
    cell_id <- raster::cellFromXY(object = seafloor,
                                  xy = fishpop_values[id_mort, c("x", "y"), drop = FALSE])

    # create new individual
    mort_temp <- create_rebirth(fishpop_values = fishpop_values[id_mort, , drop = FALSE],
                                fishpop_track = fishpop_track[[1]],
                                n_body = parameters$pop_n_body,
                                want_reserves = parameters$pop_want_reserves,
                                detritus_pool = seafloor_values[cell_id, "detritus_pool"],
                                detritus_dead = seafloor_values[cell_id, "detritus_dead"],
                                reason = "background")

    # update data frames
    fishpop_values[id_mort, ] <- mort_temp$fishpop_values

    # update the detritus pool values
    seafloor_values[cell_id, "detritus_pool"] <- mort_temp$detritus_pool

    seafloor_values[cell_id, "detritus_dead"] <- mort_temp$detritus_dead

  }

  return(list(seafloor = seafloor_values, fishpop_values = fishpop_values))
}
