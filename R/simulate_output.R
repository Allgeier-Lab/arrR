#' simulate_output
#'
#' @description Simulate nutrient output
#'
#' @param seafloor_values Matrix with seafloor values.
#' @param parameters List with all model parameters.
#'
#' @details
#' Simulate loss of nutrients to to output of the system.
#'
#' @return matrix
#'
#' @aliases simulate_output
#' @rdname simulate_output
#'
#' @export
simulate_output <- function(seafloor_values, parameters) {

  # diffuse values and save result
  rcpp_remove_output(seafloor = seafloor_values,
                     nutrients_output = parameters$nutrients_output)

}

