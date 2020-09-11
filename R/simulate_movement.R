#' simulate_movement
#'
#' @description Simulate movement of population.
#'
#' @param fish_population Data frame population created
#' with \code{\link{setup_fish_population}}.
#' @param reef_dist RasterLayer with distance to reef.
#' @param parameters List with all model parameters.
#' @param extent Vector with number of rows and columns (spatial extent).
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

  # MH: Why is this not a parameter?
  variance <- 5

  # calculate distribution for normal dist of movement distances
  # MH: Just sample from normal distribution
  mean_x <- log(parameters$pop_mean_move ^ 2 / sqrt(variance + parameters$pop_mean_move ^ 2))
  sd_x <- sqrt(log(1 + (variance / (parameters$pop_mean_move ^ 2))))

  # create random movement distance and angel in degree
  move_dist <- exp(stats::rnorm(n = nrow(fish_population), mean = mean_x, sd = sd_x))
  move_angle <- rep(x = 0, times = nrow(fish_population)) #(n = nrow(fish_population), min = 0, max = 360)

  # move towards reef
  if (reef_attraction & nrow(coords_reef) > 0) {

    heading_left <- cbind(fish_population$x +
                            (1 * cos(fish_population$heading + -45 * (pi / 180))),
                          fish_population$y +
                            (1 * sin(fish_population$heading + -45 * (pi / 180))))

    heading_straight <- cbind(fish_population$x +
                                (1 * cos(fish_population$heading + 0 * (pi / 180))),
                              fish_population$y +
                                (1 * sin(fish_population$heading + 0 * (pi / 180))))

    heading_right <- cbind(fish_population$x +
                             (1 * cos(fish_population$heading + 45 * (pi / 180))),
                           fish_population$y +
                             (1 * sin(fish_population$heading + 45 * (pi / 180))))

    heading_left <- raster::extract(x = reef_dist, y = heading_left)
    heading_straight <- raster::extract(x = reef_dist, y = heading_straight)
    heading_right <- raster::extract(x = reef_dist, y = heading_right)

    move_angle[which(heading_left < heading_straight)] <- -45
    move_angle[which(heading_right < heading_straight)] <- 45
  }

  # move individuals
  fish_population$x <- fish_population$x +
    (move_dist * cos(fish_population$heading + move_angle * (pi / 180)))

  fish_population$y <- fish_population$y +
    (move_dist * sin(fish_population$heading + move_angle * (pi / 180)))

  fish_population$heading <- stats::runif(n = nrow(fish_population),
                                          min = 0, max = 360)

  # Torus edge correction at boundaries
  fish_population$x[which(fish_population$x < extent[1])] <-
    extent[2] - (extent[1] - fish_population$x[which(fish_population$x < extent[1])])

  fish_population$x[which(fish_population$x > extent[2])] <-
    extent[1] + (fish_population$x[which(fish_population$x > extent[2])] - extent[2])

  fish_population$y[which(fish_population$y < extent[3])] <-
    extent[4] - (extent[3] - fish_population$y[which(fish_population$y < extent[3])])

  fish_population$y[which(fish_population$y > extent[4])] <-
    extent[3] + (fish_population$y[which(fish_population$y > extent[4])] - extent[4])

  # update activity

  # MH: Are there () missing?
  # MH: Why is + 1 added to mean move?
  # MH: Linear relationship between movement and activity not less than 1
  # MH: Should be basically higher if they swim more (ask Jake)
  fish_population$activity <-  (1 / (parameters$pop_mean_move + 1)) * move_dist + 1

  return(fish_population)
}
