#' simulate_movement
#' #KSM changing things here
#'
#' @description Simulate movement of population.
#'
#' @param fishpop_values Matrix with fish population created.
#' @param pop_n Numeric with number of individuals.
#' @param seafloor,seafloor_values RasterBrick and matrix with seafloor values.
#' @param coords_reef 2-column matrix with coordinates of AR.
#' @param reef_attraction If TRUE, individuals are attracted to AR.
#' @param extent Spatial extent object of the seafloor raster
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
#'
#' KSM: this is where I will add in the kurtosis function
#' KSM: need to annotate code and undertand what each step is doing
#'
#'
simulate_movement <- function(fishpop_values, pop_n, seafloor, seafloor_values,
                              coords_reef, reef_attraction, extent, parameters) {

  # extent must be vector for rcpp
  extent <- as.vector(extent, mode = "numeric")

  # calc mean of log-norm distribution
  norm_mean <- log((parameters$pop_mean_move ^ 2) /
                     sqrt(parameters$pop_mean_move ^ 2 + parameters$pop_var_move))

  # calc sd of log-norm distribution
  norm_sd <- sqrt(log(1 + (parameters$pop_var_move / (parameters$pop_mean_move ^ 2))))

  # get random numbers from log-norm distribution
  norm_random <- stats::rnorm(n = pop_n,
                              mean = norm_mean, sd = norm_sd)

  # calculate body length based on random number
  move_dist <- exp(norm_random)

  # move towards reef
  ## change this if statement to be if the doggy bag is at the max or not
  ## reef attraction is more based on consumption

  if (reef_attraction & nrow(coords_reef) > 0) {

    # get coordinates within visibility left, straight in right of individuals
    heading_l <- cbind(fishpop_values[, "x"] + (parameters$pop_visibility *
                                              cos(((fishpop_values[, "heading"] + -45) %% 360) * (pi / 180))),
                       fishpop_values[, "y"] + (parameters$pop_visibility *
                                              sin(((fishpop_values[, "heading"] + -45) %% 360) * (pi / 180))))

    heading_s <- cbind(fishpop_values[, "x"] + (parameters$pop_visibility *
                                              cos(fishpop_values[, "heading"] * (pi / 180))),
                       fishpop_values[, "y"] + (parameters$pop_visibility *
                                              sin(fishpop_values[, "heading"] * (pi / 180))))

    heading_r <- cbind(fishpop_values[, "x"] + (parameters$pop_visibility *
                                              cos(((fishpop_values[, "heading"] + 45) %% 360) * (pi / 180))),
                       fishpop_values[, "y"] + (parameters$pop_visibility *
                                              sin(((fishpop_values[, "heading"] + 45) %% 360) * (pi / 180))))

    # combine to one matrix
    heading_full <- rbind(heading_l, heading_s, heading_r)

    # torus translation if coords are outside plot
    rcpp_translate_torus(coords = heading_full, extent = extent)

    # get cell id in directions
    cell_id <- raster::cellFromXY(object = seafloor, xy = heading_full)

    # create distance matrix with left, straight right distance to reef
    dist_values <- matrix(data = seafloor_values[cell_id, "reef_dist"],
                          ncol = 3, nrow = pop_n)

    # turn fish
    rcpp_turn_fish(fishpop = fishpop_values,
                   dist_values = dist_values)

  }

  # calculate new coordinates and activity
  rcpp_move_fishpop(fishpop = fishpop_values,
                    move_dist = move_dist,
                    extent = extent,
                    pop_mean_move = parameters$pop_mean_move)

}
