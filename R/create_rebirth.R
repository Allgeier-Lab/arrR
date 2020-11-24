#' create_rebirth
#'
#' @description Internal function to create new fish individual
#'
#' @param fishpop_values,fishpop_track Matrix with current fish population
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
#' @return matrix
#'
#' @aliases create_rebirth
#' @rdname create_rebirth
#'
#' @export
create_rebirth <- function(fishpop_values, fishpop_track, n_body,
                           want_reserves, detritus_pool, detritus_dead, reason) {

  # extract values that could not be overwritten
  fishpop_values_coords <- fishpop_values[, c("x", "y"), drop = FALSE]

  counter_died <- fishpop_values[, c("died_consumption", "died_background"), drop = FALSE]

  # get starting values of individual
  starting_id <- fishpop_track[, "id"] %in% fishpop_values[, "id"]

  fishpop_start <- fishpop_track[starting_id, , drop = FALSE]

  # calculate mass difference + reserves
  mass_diff <- ((fishpop_values[, "weight"] - fishpop_start[, "weight"]) * n_body) +
    fishpop_values[, "reserves"]

  # add to dead detritus pool
  detritus_dead <- detritus_dead + mass_diff

  # create new individual
  fishpop_values <- fishpop_start

  # keep old coordinates
  fishpop_values[, c("x", "y")] <- fishpop_values_coords

  # calculate wanted reserves
  reserves_wanted <- n_body * fishpop_values[, "weight"] * want_reserves

  # check if detritus pool is larger than reserves_wanted
  id_reserves_part <- which(reserves_wanted >= detritus_pool)

  id_reserves_full <-  which(reserves_wanted < detritus_pool)

  # detritus pool is smaller than wanted reserves, detritus is fully used
  if (length(id_reserves_part) > 0) {

    fishpop_values[id_reserves_part, "reserves"] <- detritus_pool[id_reserves_part]

    detritus_pool[id_reserves_part] <- 0

  }

  # detritus pool is larger than what is wanted, reserves_wanted are used
  if (length(id_reserves_full) > 0) {

    fishpop_values[id_reserves_full, "reserves"] <- reserves_wanted[id_reserves_full]

    detritus_pool[id_reserves_full] <- detritus_pool[id_reserves_full] -
      reserves_wanted[id_reserves_full]

  }

  # increase counter died
  if (reason == "consumption") {

    fishpop_values[, "died_consumption"] <- counter_died[, "died_consumption"] + 1

    fishpop_values[, "died_background"] <- counter_died[, "died_background"]

  } else if (reason == "background") {

    fishpop_values[, "died_consumption"] <- counter_died[, "died_consumption"]

    fishpop_values[, "died_background"] <- counter_died[, "died_background"] + 1
  } else {

    stop("Please select either reason = 'consumption' or reason = 'background'.",
         call. = FALSE)

  }

  return(list(fishpop_values = fishpop_values,
              detritus_pool = detritus_pool, detritus_dead = detritus_dead))
}
