#' simulate_respiration
#'
#' @description Simulate movement of population.
#'
#' @param fish_population Data frame population created with \code{\link{setup_fish_population}}.
#' @param parameters List with all model parameters.
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
simulate_respiration <- function(fish_population, parameters, water_temp, min_per_i) {

  # MH: Why are none of these values parameters?
  resp_intercept <- parameters$resp_intercept * (1 / 24) * (1 / 60 ) * min_per_i

  # for f(T) temperature dependence function for respiration
  v_resp <- (parameters$resp_temp_max - water_temp) /
    (parameters$resp_temp_max - parameters$resp_temp_optm)

  z_resp <- log(parameters$resp_temp_low) *
    (parameters$resp_temp_max - parameters$resp_temp_optm)

  y_resp <- log(parameters$resp_temp_low) *
    (parameters$resp_temp_max - parameters$resp_temp_optm + 2)

  x_resp <- (z_resp ^ 2 * (1 + (1 + 40 / y_resp) ^ 0.5 ) ^ 2) / 400

  # ;this is the f(t) equation 2 ()
  temp_dependence <- v_resp ^ x_resp * exp(x_resp * (1 - v_resp))

  # calculate respiration
  # Oxycaloric coefficient in J/gO2 consumed multiplied by the energy-density of fish
  # to result in unit of tick^-1
  respiration <- (resp_intercept * fish_population$weight ^ parameters$resp_slope *
                    temp_dependence * fish_population$activity) * 13560 * (1 / 4800)

  # check if finite number
  respiration <- ifelse(test = !is.finite(respiration), yes = 1, no = respiration)

  # update respiration col
  fish_population$respiration <- respiration

  return(fish_population)
}
