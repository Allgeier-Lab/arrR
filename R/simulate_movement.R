#' simulate_movement
#'
#' @description Simulate movement of population.
#'
#' @param fish_population Data frame population created
#' with \code{\link{setup_fish_population}}.
#' @param reef_dist RasterLayer with distance to reef.
#' @param parameters List with all model parameters.
#' @param extent Spatial extent object of the seafloor RasterBr.
#' @param reef_attraction If TRUE, individuals are attracted to AR.
#' @param coords_reef 2-column matrix with coordinates of AR.
#'
#' @details
#' Function to simulate movement of population individuals.
#'
#' @return data.frame
#'
#' @aliases simulate_movement
#' @rdname simulate_movement
#'
#' @export
simulate_movement <- function(fish_population, reef_dist, coords_reef,
                              extent, parameters, reef_attraction) {

  # calc mean of log-norm distribution
  norm_mean <- log((parameters$pop_mean_move ^ 2) /
                     sqrt(parameters$pop_mean_move ^ 2 + parameters$pop_var_move))

  # calc sd of log-norm distribution
  norm_sd <- sqrt(log(1 + (parameters$pop_var_move / (parameters$pop_mean_move ^ 2))))

  # get random numbers from log-norm distribution
  norm_random <- stats::rnorm(n = nrow(fish_population),
                              mean = norm_mean, sd = norm_sd)

  # calculate body length based on random number
  move_dist <- exp(norm_random)

  # move towards reef
  if (reef_attraction & nrow(coords_reef) > 0) {

    # get coordinates within visibility left, straight in right of individuals
    heading_l <- cbind(fish_population$x + (parameters$pop_visibility *
                                              cos((fish_population$heading + -45) * (pi / 180))),
                       fish_population$y + (parameters$pop_visibility *
                                              sin((fish_population$heading + -45) * (pi / 180))))

    heading_s <- cbind(fish_population$x + (parameters$pop_visibility *
                                              cos(fish_population$heading * (pi / 180))),
                       fish_population$y + (parameters$pop_visibility *
                                              sin(fish_population$heading * (pi / 180))))

    heading_r <- cbind(fish_population$x + (parameters$pop_visibility *
                                              cos((fish_population$heading + 45) * (pi / 180))),
                       fish_population$y + (parameters$pop_visibility *
                                              sin((fish_population$heading + 45) * (pi / 180))))

    # torus translation if coords are outside plot
    heading_l <- int_translate_torus(coords = heading_l, extent = extent)

    heading_s <- int_translate_torus(coords = heading_s, extent = extent)

    heading_r <- int_translate_torus(coords = heading_r, extent = extent)

    # get distance values in directions
    dist_values <- raster::extract(x = reef_dist,
                                   y = rbind(heading_l, heading_s, heading_r))

    # get ids of fish that turn one direction
    id_l <- which(dist_values[1:10] < dist_values[11:20])

    id_r <- which(dist_values[21:30] < dist_values[11:20])

    # turn fish heading towards reef
    fish_population$heading[id_l] <- fish_population$heading[id_l] - 45

    fish_population$heading[id_r] <- fish_population$heading[id_r] + 45

  }

  # move individuals
  fish_population$x <- fish_population$x +
    (move_dist * cos(fish_population$heading * (pi / 180)))

  fish_population$y <- fish_population$y +
    (move_dist * sin(fish_population$heading * (pi / 180)))

  # torus edge correction at boundaries
  fish_population[, c("x", "y")] <-
    int_translate_torus(coords = fish_population[, c("x", "y")], extent = extent)

  # turn fish randomly after moving
  # MH: This could be correlated to heading; runif(min = heading - x, max = heading + x)
  fish_population$heading <- stats::runif(n = nrow(fish_population),
                                          min = 0, max = 360)

  # update activity
  fish_population$activity <-  (1 / (parameters$pop_mean_move + 1)) * move_dist + 1

  return(fish_population)
}
