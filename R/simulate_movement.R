#' simulate_movement
#'
#' @description Simulate movement of population.
#'
#' @param fish_population Data frame population created with \code{\link{setup_fish_population}}.
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
simulate_movement <- function(fish_population, parameters, extent,
                              coords_reef, reef_attraction) {

  # MH: Why is this not a parameter?
  variance <- 5

  # calculate distribution for normal dist of movement distances
  mean_x <- log(parameters$pop_mean_move ^ 2 / sqrt(variance + parameters$pop_mean_move ^ 2))
  sd_x <- sqrt(log( 1 + (variance / (parameters$pop_mean_move ^ 2))))

  # create random movement distance and angel in degree
  move_dist <- exp(stats::rnorm(n = nrow(fish_population), mean = mean_x, sd = sd_x))
  move_angle <- stats::runif(n = nrow(fish_population), min = 0, max = 360)

  # move towards reef
  if (reef_attraction) {

    # get distance to nearest reef
    reef_dist <- int_calc_dist_reefs(fish_population = fish_population[, c("x", "y")],
                                     coords_reef = coords_reef)

    # which individuals are not more than 10 m from a reef
    # MH: This is also something to explore as a parameter
    attract_id <- which(reef_dist$dist < 10)

    if (length(attract_id) > 0) {

      # which are the corresponding reef cells
      reef_id <- reef_dist$counter[attract_id]

      # calculate bearing between individuals and reef cells
      theta <- atan2(coords_reef[reef_id, 2] - fish_population$y[attract_id],
                     coords_reef[reef_id, 1] - fish_population$x[attract_id])

      # correct for 4 quadrants of coordinate system
      theta[theta < 0] <- theta[theta < 0] + 2 * pi

      # add some random deviation from straight line
      # MH: This could explored as parameter
      theta_lo <- theta * 0.75
      theta_hi <- theta * 1.25

      # # add some random deviation to distance
      # # MH: This could explored as parameter
      # dist_lo <- reef_dist$dist[attract_id] * 0.75
      # dist_hi <- reef_dist$dist[attract_id] * 1.25

      # convert to degree and replace angle
      move_angle[attract_id] <- stats::runif(n = length(attract_id),
                                             min = theta_lo, max = theta_hi) * (180 / pi)

      # # replace distance
      # move_dist[attract_id] <- runif(n = length(attract_id),
      #                                min = dist_lo, max = dist_hi)

      # MH: How to move away from reef?

    }
  }

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
