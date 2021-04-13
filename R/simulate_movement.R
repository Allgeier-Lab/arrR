#' simulate_movement
#'
#' @description Simulate movement of population.
#'
#' @param fishpop_values Matrix with fish population created.
#' @param pop_n Numeric with number of individuals.
#' @param coords_reef Matrix with coords of reef cells.
#' @param pop_thres_reserves Vector with proportion of max_reserves to drain prior to movement.
#' @param parameters List with all model parameters.
#' @param extent,dimensions Spatial extent and dimensions of the seafloor raster
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
simulate_movement <- function(fishpop_values, pop_n, coords_reef,
                              pop_thres_reserves, parameters,
                              extent, dimensions) {

  # calculate new coordinates and activity
  rcpp_move_fishpop(fishpop = fishpop_values,
                    coords_reef = coords_reef,
                    pop_thres_reserves = pop_thres_reserves,
                    move_border = parameters$move_border,
                    move_mean = parameters$move_mean,
                    move_reef = parameters$move_reef,
                    move_return = parameters$move_return,
                    extent = as.vector(extent, mode = "numeric"),
                    dimensions = dimensions)
}
