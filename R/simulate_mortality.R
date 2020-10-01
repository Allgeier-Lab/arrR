#' simulate_mortality
#'
#' @description Simulate background mortality of population.
#'
#' @param fish_population,fish_population_track Data frame population created with \code{\link{setup_fish_population}}.
#' @param seafloor RasterBrick with environment created with \code{\link{setup_seafloor}}.
#' @param parameters List with all model parameters.
#' @param min_per_i Integer to specify minutes per i.
#'
#' @details
#' Function to simulate background mortality of population individuals.
#'
#' @return data.frame
#'
#' @aliases simulate_mortality
#' @rdname simulate_mortality
#'
#' @export
simulate_mortality <- function(fish_population, fish_population_track, seafloor,
                               parameters, min_per_i) {

  # create death probability
  death_prob <- exp(fish_population$length - parameters$pop_max_size)

  # create random number to test death prob against
  random_prob <- stats::runif(n = nrow(fish_population), min = 0, max = 1)

  # identify who dies
  mort_id <- which(random_prob < death_prob)

  # check if mortality occurs
  if (length(mort_id) > 0) {

    # get detritus/nutrient pools at location
    pools <- raster::extract(x = raster::subset(seafloor,
                                                subset = c("detritus_pool",
                                                           "detritus_dead")),
                             y = fish_population[mort_id, c("x", "y")],
                             cellnumbers = TRUE)

    # loop through all dying individuals
    # MH: This could be vectorized but would need changes in int_rebirth
    # MH: Only very few individuals each time, so loop might not be a problem
    for (i in 1:length(mort_id)) {

      # create new individual
      fish_pop_temp <- int_rebirth(fish_population = fish_population[mort_id[i], ],
                                   fish_population_track = fish_population_track[[1]],
                                   n_body = parameters$pop_n_body,
                                   want_reserves = parameters$pop_want_reserves,
                                   detritus_pool = pools[[i, "detritus_pool"]],
                                   detritus_dead = pools[[i, "detritus_dead"]],
                                   reason = "background")

      # update data frames
      fish_population[i, ] <-  fish_pop_temp$fish_population

      # update detritus
      pools[i, "detritus_pool"] <- fish_pop_temp$detritus_pool

      pools[i, "detritus_dead"] <- fish_pop_temp$detritus_dead

    }

    # update the detritus pool values
    raster::values(seafloor)[pools[, "cells"], c("detritus_pool",
                                                 "detritus_dead")] <- pools[, -1]

  }

  return(list(seafloor = seafloor, fish_population = fish_population))
}
