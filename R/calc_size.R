#' calc_size
#'
#' @description
#' Calculate dimensions of fish individuals.
#'
#' @param pop_n Numeric with starting values for number of individuals.
#' @param pop_mean_size,pop_sd_size Numeric with parameters for mean size
#' and variance.
#' @param pop_linf,pop_a,pop_b Numeric with parameters for weight calculation.
#' @param use_log Logical if TRUE, random log distribution is used.
#'
#' @details
#' Calculate size and weight of fish individuals based on length-weight relationships
#' (Froese & Pauly 2019). The starting length is randomly drawn from a log-norm or
#' uniform distribution.
#'
#' If an uniform distribution is used, the minimum and maximum are based on 10%
#' and 90% of the maximum size (\code{pop_linf}), respectively.
#'
#' @references
#' Froese, R., Pauly, D., 2019. FishBase. World Wide Web electronic publication [WWW Document].
#' <www.fishbase.org>
#'
#' The LENGTH-WEIGHT Table.
#' <https://www.fishbase.org/manual/fishbasethe_length_weight_table.htm>
#'
#' @return list
#'
#' @examples
#' \dontrun{
#' calc_size(pop_n = 8, pop_mean_size = default_starting$pop_mean_size,
#' pop_sd_size = default_starting$pop_sd_size,
#' pop_linf = default_parameters$pop_linf, pop_a = default_parameters$pop_a,
#' pop_b = default_parameters$pop_b, use_log = TRUE)
#' }
#'
#' @aliases calc_size
#' @rdname calc_size
#'
#' @keywords internal
calc_size <- function(pop_n, pop_mean_size, pop_sd_size,
                      pop_linf, pop_a, pop_b, use_log = TRUE) {

  # use log distribution for starting size
  if (use_log) {

    body_length <- vapply(X = 1:pop_n, FUN = function(i) {
      rcpp_rlognorm(mean = pop_mean_size[i], sd = pop_sd_size[i],
                    min = 3.0, max = pop_linf[i] * 0.75)}, FUN.VALUE = numeric(1))

  # use uniform distribution for starting size
  } else {

    body_length <- vapply(X = 1:pop_n, FUN = function(i) {
      rcpp_runif(min = 3.0, max = pop_linf[i] * 0.75)}, FUN.VALUE = numeric(1))

  }

  # calculate weight of individuals based on body length
  weight <- pop_a * (body_length ^ pop_b)

  return(list(length = body_length, weight = weight))
}
