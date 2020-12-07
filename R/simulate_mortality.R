#' simulate_mortality
#'
#' @description Simulate background mortality of population.
#'
#' @param fish_population,fish_population_track Data frame population created
#' with \code{\link{setup_fish_population}}.
#' @param n_pop Numeric with number of individuals.
#' @param seafloor,seafloor_values RasterLayer and data.frame with seafloor values.
#' @param parameters List with all model parameters.
#' @param min_per_i Integer to specify minutes per i.
#'
#' @details
#' Function to simulate background mortality of fish population individuals.
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

  # sample random ordering of individuals
  if (n_pop > 1) {

    fish_id <- sample(x = fish_population$id, size = n_pop)

  } else {

    fish_id <- fish_population$id

  }

  # get detritus/nutrient pools at location and raster cells
  cell_id <- raster::cellFromXY(object = seafloor,
                                xy = fish_population[fish_id, c("x", "y")])

  # loop through all dying individuals
  for (i in seq_along(fish_id)) {

    # create counter for temp fish id
    fish_id_temp <- fish_id[i]

    # create counter for temp cell id
    cell_id_temp <- cell_id[i]

    # create death probability
    death_prob <- exp(fish_population$length[fish_id_temp] - parameters$pop_max_size)

    # create random number to test death prob against
    random_prob <- stats::runif(n = 1, min = 0, max = 1)

    # identify who dies
    if (random_prob < death_prob) {

      # create new individual
      fish_pop_temp <- create_rebirth(fish_population = fish_population[fish_id_temp, ],
                                      fish_population_track = fish_population_track[[1]],
                                      n_body = parameters$pop_n_body,
                                      want_reserves = parameters$pop_want_reserves,
                                      detritus_pool = seafloor_values$detritus_pool[cell_id_temp],
                                      detritus_dead =  seafloor_values$detritus_dead[cell_id_temp],
                                      reason = "background")

      # update data frames
      fish_population[fish_id_temp, ] <- fish_pop_temp$fish_population

      # update the detritus pool values
      seafloor_values$detritus_pool[cell_id_temp] <- fish_pop_temp$detritus_pool

      seafloor_values$detritus_dead[cell_id_temp] <- fish_pop_temp$detritus_dead

    }
  }

  return(list(seafloor = seafloor_values, fish_population = fish_population))
}
