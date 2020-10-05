#' simulate_mortality
#'
#' @description Simulate background mortality of population.
#'
#' @param fish_population,fish_population_track Data frame population created with \code{\link{setup_fish_population}}.
#' @param n_pop Numeric with number of individuals.
#' @param seafloor,seafloor_values RasterLayer and data.frame with seafloor values.
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
simulate_mortality <- function(fish_population, fish_population_track,
                               seafloor, seafloor_values,
                               n_pop, parameters, min_per_i) {

  # create death probability
  death_prob <- exp(fish_population$length - parameters$pop_max_size)

  # create random number to test death prob against
  random_prob <- stats::runif(n = n_pop, min = 0, max = 1)

  # identify who dies
  mort_id <- which(random_prob < death_prob)

  # check if mortality occurs
  if (length(mort_id) > 0) {

    # get detritus/nutrient pools at location and raster cells
    cell_id <- raster::cellFromXY(object = seafloor,
                                  xy = fish_population[mort_id, c("x", "y")])

    detritus_pool <- seafloor_values$detritus_pool[cell_id]

    detritus_dead <- seafloor_values$detritus_dead[cell_id]


    # loop through all dying individuals
    # MH: This could be vectorized but would need changes in int_rebirth
    # MH: Only very few individuals each time, so loop might not be a problem
    for (i in 1:length(mort_id)) {

      # create new individual
      fish_pop_temp <- int_rebirth(fish_population = fish_population[mort_id[i], ],
                                   fish_population_track = fish_population_track[[1]],
                                   n_body = parameters$pop_n_body,
                                   want_reserves = parameters$pop_want_reserves,
                                   detritus_pool = detritus_pool[i],
                                   detritus_dead = detritus_dead[i],
                                   reason = "background")

      # update data frames
      fish_population[mort_id[i], ] <- fish_pop_temp$fish_population

      # update detritus
      detritus_pool[i] <- fish_pop_temp$detritus_pool

      detritus_dead[i] <- fish_pop_temp$detritus_dead

    }

    # update the detritus pool values
    seafloor_values$detritus_pool[cell_id] <- detritus_pool

    seafloor_values$detritus_dead[cell_id] <- detritus_dead

  }

  return(list(seafloor = seafloor_values, fish_population = fish_population))
}
