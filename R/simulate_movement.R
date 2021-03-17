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
#' @param prop_reserves Double with proportion of max_reserves to drain prior to movement.
#' @param reef_mean_move Double with limited movement at reef.
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

  # calc mean of log-norm distribution
  norm_mean <- log((parameters$move_mean ^ 2) /
                     sqrt(parameters$move_mean ^ 2 + parameters$move_var))

  # calc sd of log-norm distribution
  norm_sd <- sqrt(log(1 + (parameters$move_var / (parameters$move_mean ^ 2))))

  # get random numbers from log-norm distribution
  norm_random <- stats::rnorm(n = pop_n,
                              mean = norm_mean, sd = norm_sd)

  # calculate movement distance based on random number
  move_dist <- exp(norm_random)

  # calculate new coordinates and activity
  rcpp_move_fishpop(fishpop = fishpop_values,
                    reef_dist = seafloor_values[, "reef_dist"],
                    move_dist = move_dist,
                    move_mean = parameters$move_mean,
                    pop_visibility = parameters$pop_visibility,
                    pop_reserves = parameters$pop_reserves,
                    move_reef = parameters$move_reef,
                    extent = extent,
                    dimensions = dimensions)
}
