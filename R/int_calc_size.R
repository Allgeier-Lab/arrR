#' calc_size
#'
#' @description Internal function
#'
#' @param starting_values List with all starting value parameters.
#' @param parameters List with all model parameters.
#'
#' @details
#' Internal function to calculate size and weight of individuals.
#'
#' @return vector
#'
#' @aliases int_calc_size
#' @rdname int_calc_size
#'
#' @keywords internal
#'
#' @export
int_calc_size <- function(starting_values, parameters) {

  # MH: Why is the variance set to v = 10?
  variance <- 10

  # calc mean of log-norm distribution
  norm_mean <- log((parameters$pop_mean_size ^ 2) /
                     sqrt(parameters$pop_mean_size ^ 2 + variance))

  # calc sd of log-norm distribution
  norm_sd <- sqrt(log(1 + (variance / (parameters$pop_mean_size ^ 2))))

  # get random numbers from log-norm distribution
  norm_random <- stats::rnorm(n = starting_values$pop_n,
                              mean = norm_mean, sd = norm_sd)

  # calculate body length based on random number
  body_length <- exp(norm_random)

  # calculate weight of individuals based on body length
  weight <- parameters$pop_a_grunt * (body_length ^ parameters$pop_b_grunt)

  return(list(length = body_length, weight = weight))
}
