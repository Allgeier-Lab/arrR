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
  detritus_pool <- raster::extract(x = seafloor$detritus_pool, y = fish_population[, c("x", "y")])
  detritus_dead <- raster::extract(x = seafloor$detritus_dead, y = fish_population[, c("x", "y")])

  death_prob <- exp(1 * (fish_population$length - 45)) / 120

  random_prob <- stats::runif(n = nrow(fish_population), min = 0, max = 1)

  id_dying <- which(random_prob < death_prob)

  if (length(id_dying) > 0) {

    # get starting values of individual
    indiv_starting_values <- subset(fish_population_track, id %in% id_dying & track_i == 0)

    # calculate mass difference + reserves
    mass_diff <- (fish_population$weight[id_dying] + fish_population$reserves[id_dying]) -
      indiv_starting_values$weight

    # add to dead detritus pool
    detritus_dead[id_dying] <- mass_diff

    fish_population[id_dying ,] <- indiv_starting_values[,-19]

    # divide starting reserves by 5 because here the formula is multiplied
    # by 0.01 and 0.05 as during setup
    reserves_wanted <- fish_population$reserves[id_dying] / 5

    # if more reserver are wanted than availaible, all are used
    if (reserves_wanted >= detritus_pool[id_dying]) {

      fish_population$reserves[id_dying] <- detritus_pool[id_dying]

      detritus_pool[id_dying] <- 0

      # pool is larger than what is wanted, so only subset is used
    } else {

      fish_population$reserves[id_dying] <- reserves_wanted

      detritus_pool[id_dying] <- detritus_pool[id_dying] - reserves_wanted

    }

    # increase counter died
    fish_population$died[id_dying] <- fish_population$died[id_dying] + 1

  }

  return(fish_population)
}
