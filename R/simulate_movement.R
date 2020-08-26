#' simulate_movement
#'
#' @description Simulate movement of population.
#'
#' @param environment RasterBrick with environment created with \code{\link{setup_environment}}.
#' @param population Data frame population created with \code{\link{setup_population}}.
#' @param mean_move Numeric with mean move parameter of individuals.
#' @param extent Vector with number of rows and columns (spatial extent).
#' @param reef_attraction If TRUE, individuals are attracted to AR.
#' @param verbose If TRUE, progress reports are printed.
#'
#' @details
#' Function to simulate movement of population individuals.
#'
#' @return data.table
#'
#' @aliases simulate_movement
#' @rdname simulate_movement
#'
#' @export
simulate_movement <- function(environment, population, mean_move,extent,
                              reef_attraction, verbose = TRUE) {

  if (verbose) {

    message("Simulating movement of individuals...")

  }

  # MH: Why is this not a parameter?
  variance <- 5

  # calculate distribution for normal dist of movement distances
  mean_x <- log(mean_move ^ 2 / sqrt(variance + mean_move ^ 2))
  sd_x <- sqrt(log( 1 + (variance / (mean_move ^ 2))))

  # create random movement distance and angel in degree
  move_dist <- exp(stats::rnorm(n = nrow(population), mean = mean_x, sd = sd_x))
  move_angle <- stats::runif(n = nrow(population), min = 0, max = 360)

  # move individuals
  # MH: In NetLogo individuals only turn between 0 to 25° all the time?
  population[, x := x + (move_dist * cos(move_angle * (pi / 180)))]
  population[, y := y + (move_dist * sin(move_angle * (pi / 180)))]

  # Torus at edges
  population[x < extent[1], x := extent[2] - (extent[1] - x)]
  population[x > extent[2], x := extent[1] + (x - extent[2])]

  population[y < extent[3], y := extent[4] - (extent[3] - y)]
  population[y > extent[4], y := extent[3] + (y - extent[4])]

  return(population)
}
