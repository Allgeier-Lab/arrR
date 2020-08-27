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
#' @aliases calc_size
#' @rdname calc_size
#'
#' @keywords internal
#'
#' @export
int_calc_size <- function(starting_values, parameters) {

  # MH: Why is the variance set to v = 10?
  variance <- 10

  # MH: Where is this function coming from?
  norm_mean <- log((parameters$pop_mean_size ^ 2) / sqrt(variance + (parameters$pop_mean_size ^ 2)))
  norm_sd <- sqrt(log(1 + (variance / (parameters$pop_mean_size ^ 2))))

  # get random numbers from norm distribution
  norm_random <- stats::rnorm(n = starting_values$pop_n, mean = norm_mean, sd = norm_sd)

  # calculate body length based on random number
  # MH: Why is the body length the exp of the random number?
  body_length <- exp(norm_random)

  # set body length to minimum 20
  # MH: Is that even mathematical possible? I guess it depends on norm_mean and norm_sd
  body_length <- ifelse(test = body_length == 0, yes = 20, no = body_length)

  # calculate weight of individuals based on body length
  # MH: Where is this formula coming from?
  weight <- parameters$pop_a_grunt * (body_length ^ parameters$pop_b_grunt)

  # # calculate the size of individuals
  # # MH: Why is size different than body length?
  # # MH: Only needed for plotting, right?
  # size <- (2 * body_length / 40)

  return(list(length = body_length, weight = weight))
}
