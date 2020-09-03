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

  # MH: This only depends on the water temp?

  # MH: Why are none of these values parameters?
  ra <- 0.0108 * (1 / 24) * (1 / 60 ) * min_per_i

  rb <- -0.2
  rq <- 2.1
  rto <- 36
  rtm <- 40

  # for f(T) temperature dependence function for respiration
  vr <- (rtm - water_temp) / (rtm - rto)
  zr <- log(rq) * (rtm - rto)
  yr <- log(rq) * (rtm - rto + 2)
  xr <- (zr ^ 2 * (1 + (1 + 40 / yr) ^ 0.5 ) ^ 2) / 400
  # ;this is the f(t) equation 2 ()
  f_tr <- vr ^ xr * exp(xr * (1 - vr))

  # update respiration col
  # MH: Why multiplied by 13560 etc.?
  fish_population$respiration <- (ra * fish_population$weight ^ rb * f_tr *
                                    fish_population$activity) * 13560 * (1 / 4800)

  return(fish_population)
}
