#' create_rebirth
#'
#' @description Internal function to create new fish individual
#'
#' @param fish_population,fish_population_track Data.frame with current fish population
#' and fish population of first time step.
#' @param n_body,want_reserves Numeric with parameters to calculate reserves.
#' @param detritus_pool,detritus_dead Vector with detritus values at location of
#' individuals.
#' @param reason Character specifying reason of death ('consumption' or 'background').
#'
#' @details
#' Internal function to create new individual with same starting values as at timestep=0
#' if an individual dies.
#'
#' @return data.frame
#'
#' @aliases create_rebirth
#' @rdname create_rebirth
#'
#' @export
create_rebirth <- function(fish_population, fish_population_track, n_body,
                           want_reserves, detritus_pool, detritus_dead, reason) {

  # extract values that could not be overwritten
  fish_population_coords <- fish_population[, c("x", "y")]

  counter_died <- fish_population[, c("died_consumption", "died_background")]

  # get starting values of individual
  fish_population_start <- fish_population_track[fish_population_track$id ==
                                                   fish_population$id, ]

  # calculate mass difference + reserves
  mass_diff <- (fish_population$weight - fish_population_start$weight) *
    n_body + fish_population$reserves

  # add to dead detritus pool
  detritus_dead <- detritus_dead + mass_diff

  # create new individual
  fish_population <- fish_population_start

  # keep old coordinates
  fish_population[, c("x", "y")] <- fish_population_coords

  # calculate wanted reserves
  reserves_wanted <- n_body * fish_population$weight * want_reserves

  # if more reserves are wanted than available, all are used
  if (reserves_wanted >= detritus_pool) {

    fish_population$reserves <- detritus_pool

    detritus_pool <- 0

  # pool is larger than what is wanted, so only subset is used
  } else {

    fish_population$reserves <- reserves_wanted

    detritus_pool <- detritus_pool - reserves_wanted

  }

  # increase counter died
  if (reason == "consumption") {

    fish_population$died_consumption <- counter_died[[1]] + 1

    fish_population$died_background <- counter_died[[2]]

  } else if (reason == "background") {

    fish_population$died_consumption <- counter_died[[1]]

    fish_population$died_background <- counter_died[[2]] + 1

  } else {

    stop("Please select either reason = 'consumption' or reason = 'background'.",
         call. = FALSE)

  }

  return(list(fish_population = fish_population,
              detritus_pool = detritus_pool, detritus_dead = detritus_dead))
}
