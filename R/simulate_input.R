#' simulate_input
#'
#' @description Simulate nutrient input
#'
#' @param seafloor_values Matrix with seafloor values.
#' @param nutr_input Vector with amount of nutrient input each timestep.
#' @param timestep Integer with current timestep.
#'
#' @details
#' Simulate external nutrient input to the system each timestep. The \code{nutr_input}
#' vector must have as many elements as \code{max_i}.
#'
#' @return matrix
#'
#' @aliases simulate_input
#' @rdname simulate_input
#'
#' @export
simulate_input <- function(seafloor_values, nutr_input, timestep) {

  # diffuse values and save result
  rcpp_add_input(seafloor = seafloor_values,
                 nutr_input = nutr_input,
                 timestep = timestep)

}

