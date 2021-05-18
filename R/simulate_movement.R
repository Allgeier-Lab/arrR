#' simulate_movement
#'
#' @description Simulate movement of population.
#'
#' @param fishpop_values Matrix with fish population created.
#' @param movement String specifing movement algorithm. Either 'rand', 'attr' or 'behav'.
#' @param parameters List with all model parameters.
#' @param max_dist Maximum distance an individual can move..
#' @param pop_thres_reserves Vector with proportion of max_reserves to drain prior to movement.
#' @param coords_reef Matrix with coords of reef cells.
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
simulate_movement <- function(fishpop_values, movement = "rand",
                              parameters, pop_thres_reserves, max_dist,
                              coords_reef, extent, dimensions) {


  if (movement == "rand") {

    # calculate new coordinates and activity
    rcpp_move_rand(fishpop = fishpop_values,
                   coords_reef = coords_reef,
                   move_mean = parameters$move_mean,
                   move_var = parameters$move_var,
                   move_visibility = parameters$move_visibility,
                   max_dist = max_dist,
                   reef_attraction = FALSE,
                   extent = as.vector(extent, mode = "numeric"),
                   dimensions = dimensions)

  } else if (movement == "attr") {

    # calculate new coordinates and activity
    rcpp_move_rand(fishpop = fishpop_values,
                   coords_reef = coords_reef,
                   move_mean = parameters$move_mean,
                   move_var = parameters$move_var,
                   move_visibility = parameters$move_visibility,
                   max_dist = max_dist,
                   reef_attraction = TRUE,
                   extent = as.vector(extent, mode = "numeric"),
                   dimensions = dimensions)

  } else if (movement == "behav") {

    # calculate new coordinates and activity
    rcpp_move_behav(fishpop = fishpop_values,
                    coords_reef = coords_reef,
                    pop_thres_reserves = pop_thres_reserves,
                    move_mean = parameters$move_mean,
                    move_var = parameters$move_var,
                    move_reef = parameters$move_reef,
                    move_border = parameters$move_border,
                    move_return = parameters$move_return,
                    max_dist = max_dist,
                    extent = as.vector(extent, mode = "numeric"),
                    dimensions = dimensions)

  }
}
