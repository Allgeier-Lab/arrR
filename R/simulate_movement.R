#' simulate_movement
#'
#' @description Simulate movement of population.
#'
#' @param fish_population Data frame population created with \code{\link{setup_fish_population}}.
#' @param parameters List with all model parameters.
#' @param extent Vector with number of rows and columns (spatial extent).
#' @param reef_attraction If TRUE, individuals are attracted to AR.
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
simulate_movement <- function(fish_population, parameters, extent,
                              reef_attraction) {

  # MH: Why is this not a parameter?
  variance <- 5

  # calculate distribution for normal dist of movement distances
  mean_x <- log(parameters$pop_mean_move ^ 2 / sqrt(variance + parameters$pop_mean_move ^ 2))
  sd_x <- sqrt(log( 1 + (variance / (parameters$pop_mean_move ^ 2))))

  if (reef_attraction) {

    # add attraction towards reef cells

  }

  # create random movement distance and angel in degree
  move_dist <- exp(stats::rnorm(n = nrow(fish_population), mean = mean_x, sd = sd_x))
  move_angle <- stats::runif(n = nrow(fish_population), min = 0, max = 360)

  # move individuals
  # MH: In NetLogo individuals only turn between 0 to 25° all the time?
  fish_population$x <- fish_population$x + (move_dist * cos(move_angle * (pi / 180)))
  fish_population$y <- fish_population$y + (move_dist * sin(move_angle * (pi / 180)))

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
  fish_population$activity <-  (1 / (parameters$pop_mean_move + 1)) * move_dist + 1

  return(fish_population)
}
