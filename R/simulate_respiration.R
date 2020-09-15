#' simulate_respiration
#'
#' @description Simulate movement of population.
#'
#' @param fish_population Data frame population created with \code{\link{setup_fish_population}}.
#' @param water_temp Numeric with water temperature.
#' @param min_per_i Integer to specify minutes per i.
#'
#' @details
#' Function to simulate movement of population individuals.
#'
#' @return data.frame
#'
#' @aliases simulate_respiration
#' @rdname simulate_respiration
#'
#' @export
simulate_respiration <- function(fish_population, water_temp, min_per_i) {

  # MH: Why are none of these values parameters?
  allometric_intercept <- 0.0108 * (1 / 24) * (1 / 60 ) * min_per_i

  allometric_slope <- -0.2

  temp_low <- 2.1
  temp_optm <- 36
  temp_max <- 40

  # for f(T) temperature dependence function for respiration
  v_resp <- (temp_max - water_temp) / (temp_max - temp_optm)
  z_resp <- log(temp_low) * (temp_max - temp_optm)
  y_resp <- log(temp_low) * (temp_max - temp_optm + 2)
  x_resp <- (z_resp ^ 2 * (1 + (1 + 40 / y_resp) ^ 0.5 ) ^ 2) / 400

  # ;this is the f(t) equation 2 ()
  temp_dependence <- v_resp ^ x_resp * exp(x_resp * (1 - v_resp))

  # update respiration col
  # MH: Why multiplied by 13560 etc.?
  fish_population$respiration <-
    (allometric_intercept * fish_population$weight ^ allometric_slope *
       temp_dependence * fish_population$activity) * 13560 * (1 / 4800)

  return(fish_population)
}
