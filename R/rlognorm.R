#' rlognorm
#'
#' @description Random number from log-norm distribution.
#'
#' @param n Integer with number of generated numbers.
#' @param mean Numeric with mean value.
#' @param sd Numerich with sd value.
#'
#' @details
#' Function to simulate random number from log-normal distribution
#'
#' @return vector
#'
#' @aliases rlognorm
#' @rdname rlognorm
#'
#'@export
rlognorm <- function(n = 1, mean, sd) {

  # calc mean of log-norm distribution
  norm_mean <- log(mean ^ 2 / sqrt(mean ^ 2 + sd ^ 2))

  # calc sd of log-norm distribution
  norm_sd <- sqrt(log(1 + (sd ^ 2 / mean ^ 2)))

  # get random numbers from log-norm distribution
  norm_random <- exp(stats::rnorm(n = n, mean = norm_mean, sd = norm_sd))

  return(norm_random)

}
