#' simulate_movement
#'
#' @description Simulate movement of population.
#'
#' @param fishpop_values Matrix with fish population created.
#' @param pop_n Numeric with number of individuals.
#' @param seafloor_values RasterBrick and matrix with seafloor values.
#' @param reef_attraction If TRUE, individuals are attracted to AR.
#' @param extent,dimensions Spatial extent and dimensions of the seafloor raster
#' @param parameters List with all model parameters.
#'
#' @details
#' Function to simulate movement of fish population individuals.
#'
#' @return matrix
#'
#' @aliases simulate_movement
#' @rdname simulate_movement
#'
#' @export
simulate_movement <- function(fishpop_values, pop_n, seafloor_values,
                              reef_attraction, extent, dimensions,
                              parameters) {

  # extent must be vector for rcpp
  extent <- as.vector(extent, mode = "numeric")

  # get random move distance based on lognorm distribution
  move_dist <- rlognorm(n = pop_n, mean = parameters$pop_mean_move,
                        sd = sqrt(parameters$pop_var_move))

  # calculate new coordinates and activity
  rcpp_move_fishpop(fishpop = fishpop_values,
                    reef_dist = seafloor_values[, "reef_dist"],
                    move_dist = move_dist,
                    pop_mean_move = parameters$pop_mean_move,
                    pop_visibility = parameters$pop_visibility,
                    extent = extent,
                    dimensions = dimensions,
                    reef_attraction = reef_attraction)
}
