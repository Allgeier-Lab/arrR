#' calc_size
#'
#' @description Internal function
#'
#' @param object Raster* object
#' @param xy 2-Column matrix with coordinates of artificial reefs
#'
#' @details
#' Internal function to set biomass cell values of non-ARs cells.
#'
#' @return vector
#'
#' @aliases calc_size
#' @rdname calc_size
#'
#' @keywords internal
#'
#' @export
int_calc_size <- function(n, parameters) {

  v <- 10 # why is variance set to 10?
  u <- log(parameters$mean_size ^ 2 / sqrt(v + parameters$mean_size ^ 2))
  o <- sqrt(log(1 + (v / (parameters$mean_size ^ 2))))

  ns <- stats::rnorm(n = n, mean = u, sd = o)

  body_length <- exp(ns)

  body_length <- ifelse(test = ns == 0, yes = 20, no = body_length)

  weight <- parameters$a_grunt * (body_length ^ parameters$b_grunt)

  size <- (2 * body_length / 40)

  return(list(size = size, weight = weight))
}
