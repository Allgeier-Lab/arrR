#' calc_size
#'
#' @description Internal function
#'
#' @param pop_n Numeric with starting values for number of individuals.
#' @param pop_mean_size,pop_var_size Numeric with parameters for mean size and variance.
#' @param pop_a_grunt,pop_b_grunt Numeric with parameters for weight calculation.
#'
#' @details
#' Internal function to calculate size and weight of individuals.
#'
#' @return vector
#'
#' @aliases calc_size
#' @rdname calc_size
#'
#' @export
calc_size <- function(pop_n, pop_mean_size, pop_var_size,
                          pop_a_grunt, pop_b_grunt) {

  # calc mean of log-norm distribution
  norm_mean <- log((pop_mean_size ^ 2) / sqrt(pop_mean_size ^ 2 + pop_var_size))

  # calc sd of log-norm distribution
  norm_sd <- sqrt(log(1 + (pop_var_size / (pop_mean_size ^ 2))))

  # get random numbers from log-norm distribution
  norm_random <- stats::rnorm(n = pop_n,
                              mean = norm_mean, sd = norm_sd)

  # calculate body length based on random number
  body_length <- exp(norm_random)

  # calculate weight of individuals based on body length
  weight <- pop_a_grunt * (body_length ^ pop_b_grunt)

  return(list(length = body_length, weight = weight))
}
