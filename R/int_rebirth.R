#' rebirth
#'
#' @description Internal function
#'
#' @param fish_population,fish_population_track Data frame population created
#' with \code{\link{setup_fish_population}}.
#' @param detritus_pool,detritus_dead Vector with detritus values at location of
#' individual.
#' @param reason Character specifying reason of death ('consumption' or 'background').
#'
#' @details
#' Internal function to create new individual if mortality takes place
#'
#' @return data.frame
#'
#' @aliases int_rebirth
#' @rdname int_rebirth
#'
#' @keywords internal
#'
#' @export
int_rebirth <- function(fish_population, fish_population_track,
                        detritus_pool, detritus_dead, reason) {

  # get starting values of individual
  starting_values <- subset(fish_population_track[[1]],
                                  id == fish_population$id)

  # calculate mass difference + reserves
  mass_diff <- (fish_population$weight + fish_population$reserves) -
    starting_values$weight

  # add to dead detritus pool
  detritus_dead <- detritus_dead + mass_diff

  # create new individual with timestep tracker
  fish_population <- starting_values[, -20]

  # divide starting reserves by 5 because here the formula is multiplied
  # by 0.01 and 0.05 as during setup
  reserves_wanted <- fish_population$reserves / 5

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

    fish_population$died_consumption <- fish_population$died_consumption + 1

  } else if (reason == "background") {

    fish_population$died_background <- fish_population$died_background + 1

  } else {

    stop("Please select either reason = 'consumption' or reason = 'background'.",
         call. = FALSE)

  }

  return(list(fish_population = fish_population,
              detritus_pool = detritus_pool, detritus_dead = detritus_dead))
}
