#' calc_sigmoid
#'
#' @description Calculate sigmoid to distribute uptake.
#'
#' @param x Numeric value to convert.
#' @param what Either 'above' or 'below'.
#'
#' @details
#' Calculates sigmoid that redistributes total uptake depending on x.
#'
#' @return vector
#'
#' @examples
#' biomass <- seq(from = 0, to = 1, by = 0.01)
#' f_bg <- int_calc_sigmoid(biomass, log_slope = -2, rescale = FALSE, to = c(-1, 1))
#'
#' plot(biomass, f_bg, type = "l")
#' lines(biomass, 1 - f_bg)
#'
#' @aliases int_int_calc_sigmoid
#' @rdname int_int_calc_sigmoid
#'
#' @keywords internal
#'
#' @export
int_calc_sigmoid <- function(x, log_slope) {

  result <- (1 - (1 / (1 + ((1 / x) - 1) ^ -log_slope)))

  return(result)

}
