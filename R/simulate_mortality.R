#' simulate_mortality
#'
#' @description Simulate background mortality of population.
#'
#' @param fish_population,fish_population_track Data frame population created with \code{\link{setup_fish_population}}.
#' @param seafloor RasterBrick with environment created with \code{\link{setup_seafloor}}.
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
simulate_mortality <- function(fish_population, fish_population_track, seafloor){

  # get detritus pool at location
  # MH: This is only needed of id > 0 correct?
  detritus_pool <- raster::extract(x = seafloor$detritus_pool,
                                   y = fish_population[, c("x", "y")])

  detritus_dead <- raster::extract(x = seafloor$detritus_dead,
                                   y = fish_population[, c("x", "y")])

  # create death probability
  death_prob <- exp(1 * (fish_population$length - 45)) / 120

  # create random number to test death prob against
  random_prob <- stats::runif(n = nrow(fish_population), min = 0, max = 1)

  # identify who dies
  id <- which(random_prob < death_prob)

  # check if mortality occurs
  if (length(id) > 0) {

    # loop through all dying individuals
    # MH: This could be vectorized but would need changes in int_rebirth
    # MH: Only very few individuals each time, so loop might not be a problem
    for (i in id) {

      # create new individual
      fish_pop_temp <- int_rebirth(fish_population = fish_population[i, ],
                                   fish_population_track = fish_population_track[[1]],
                                   detritus_pool = detritus_pool[i],
                                   detritus_dead = detritus_dead[i],
                                   reason = "background")

      # update data frames
      fish_population[i, ] <-  fish_pop_temp$fish_population

      # update detritus
      detritus_pool[i] <- fish_pop_temp$detritus_pool
      detritus_dead[i] <- fish_pop_temp$detritus_dead

    }
  }

  return(fish_population)
}
