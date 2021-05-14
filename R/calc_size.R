#' calc_size
#'
#' @description Internal function to calculate size
#'
#' @param pop_n Numeric with starting values for number of individuals.
#' @param pop_mean_size,pop_linf,pop_var_size Numeric with parameters for mean size
#' and variance.
#' @param pop_a,pop_b Numeric with parameters for weight calculation.
#' @param use_log Logical if TRUE, random log distribution is used.
#'
#' @details
#' Internal function to calculate size and weight of fish individuals.
#'
#' @return vector
#'
#' @aliases calc_size
#' @rdname calc_size
#'
#' @export
calc_size <- function(pop_n, pop_mean_size, pop_linf, pop_var_size,
                      pop_a, pop_b, use_log) {

  # use log distribution for starting size
  if (use_log) {

    body_length <- rcpp_rlognorm(mean = pop_mean_size, sd = sqrt(pop_var_size),
                                 min = 0, max = Inf)

  # use uniform distribution for starting size
  } else {

    body_length <- stats::runif(n = pop_n, min = pop_linf * 0.1, max = pop_linf * 0.9)

  }

  # calculate weight of individuals based on body length
  weight <- pop_a * (body_length ^ pop_b)

  return(list(length = body_length, weight = weight))
}
