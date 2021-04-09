#' simulate_movement
#'
#' @description Simulate movement of population.
#'
#' @param fishpop_values Matrix with fish population created.
#' @param pop_n Numeric with number of individuals.
#' @param seafloor_values RasterBrick and matrix with seafloor values.
#' @param coords_reef Matrix with coords of reef cells
#' @param extent,dimensions Spatial extent and dimensions of the seafloor raster
#' @param parameters List with all model parameters.
#' @param pop_thres_reserves Vector with proportion of max_reserves to drain prior to movement.
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
simulate_movement <- function(fishpop_values, pop_n,
                              seafloor_values, coords_reef,
                              pop_thres_reserves,
                              extent, dimensions,
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

  # MH: No need to add behaviour as function argument. But we will need coords_reef (matrix) as argument

  # calculate new coordinates and activity
  rcpp_move_fishpop(fishpop = fishpop_values,
                    reef_dist = seafloor_values[, "reef_dist"],
                    coords_reef = coords_reef,
                    move_mean = parameters$move_mean,
                    pop_thres_reserves = pop_thres_reserves,
                    move_reef = parameters$move_reef,
                    move_return = parameters$move_return,
                    extent = extent,
                    dimensions = dimensions)
}
