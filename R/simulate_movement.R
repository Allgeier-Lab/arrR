#' simulate_movement
#'
#' @description Simulate movement of population.
#'
#' @param fishpop_values Matrix with fish population created.
#' @param n_pop Numeric with number of individuals.
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
simulate_movement <- function(fishpop_values, n_pop, seafloor, seafloor_values,
                              coords_reef, reef_attraction, extent, parameters) {

  # calc mean of log-norm distribution
  norm_mean <- log((parameters$pop_mean_move ^ 2) /
                     sqrt(parameters$pop_mean_move ^ 2 + parameters$pop_var_move))

  # calc sd of log-norm distribution
  norm_sd <- sqrt(log(1 + (parameters$pop_var_move / (parameters$pop_mean_move ^ 2))))

  # get random numbers from log-norm distribution
  norm_random <- stats::rnorm(n = n_pop,
                              mean = norm_mean, sd = norm_sd)

  # calculate body length based on random number
  move_dist <- exp(norm_random)

  # move towards reef
  if (reef_attraction & nrow(coords_reef) > 0) {

    # get coordinates within visibility left, straight in right of individuals
    heading_l <- cbind(fishpop_values[, "x"] + (parameters$pop_visibility *
                                              cos((fishpop_values[, "heading"] + -45) * (pi / 180))),
                       fishpop_values[, "y"] + (parameters$pop_visibility *
                                              sin((fishpop_values[, "heading"] + -45) * (pi / 180))))

    heading_s <- cbind(fishpop_values[, "x"] + (parameters$pop_visibility *
                                              cos(fishpop_values[, "heading"] * (pi / 180))),
                       fishpop_values[, "y"] + (parameters$pop_visibility *
                                              sin(fishpop_values[, "heading"] * (pi / 180))))

    heading_r <- cbind(fishpop_values[, "x"] + (parameters$pop_visibility *
                                              cos((fishpop_values[, "heading"] + 45) * (pi / 180))),
                       fishpop_values[, "y"] + (parameters$pop_visibility *
                                              sin((fishpop_values[, "heading"] + 45) * (pi / 180))))

    # torus translation if coords are outside plot and add id col
    heading_l <- translate_torus(coords = heading_l, extent = extent)

    heading_s <- translate_torus(coords = heading_s, extent = extent)

    heading_r <- translate_torus(coords = heading_r, extent = extent)

    # create id for direction
    direction_id <- rep(c("s", "l", "r"), each = n_pop)

    # combine to one matrix
    heading_full <- rbind(heading_s, heading_l, heading_r)

    # get distance values in directions
    cell_id <- raster::cellFromXY(object = seafloor, xy = heading_full)

    dist_values <- seafloor_values[cell_id, "reef_dist"]

    # get ids of fish that turn one direction
    id_l <- which(dist_values[direction_id == "l"] <
                    dist_values[direction_id == "s"])

    id_r <- which(dist_values[direction_id == "r"] <
                    dist_values[direction_id == "s"])

    # turn fish heading towards reef
    fishpop_values[id_l, "heading"] <- fishpop_values[id_l, "heading"] - 45

    fishpop_values[id_r, "heading"] <- fishpop_values[id_r, "heading"] + 45

  }

  # move individuals
  fishpop_values[, "x"] <- fishpop_values[, "x"] +
    (move_dist * cos(fishpop_values[, "heading"] * (pi / 180)))

  fishpop_values[, "y"] <- fishpop_values[, "y"] +
    (move_dist * sin(fishpop_values[, "heading"] * (pi / 180)))

  # torus edge correction at boundaries
  fishpop_values[, c("x", "y")] <-
    translate_torus(coords = fishpop_values[, c("x", "y")], extent = extent)

  # turn fish randomly after moving
  # MH: This could be correlated to heading; runif(min = heading - x, max = heading + x)
  fishpop_values[, "heading"] <- stats::runif(n = n_pop,
                                          min = 0, max = 360)

  # update activity
  fishpop_values[, "activity"] <- (1 / (parameters$pop_mean_move + 1)) * move_dist + 1

  return(fishpop_values)
}
