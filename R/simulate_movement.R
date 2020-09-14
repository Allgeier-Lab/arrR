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

  # set visibility in which fish try to minimize distance to reef
  # MH: Why is this not a parameter?
  visibility <- 1 # parameters$pop_mean_move * 0.25

  # set variance of movement distance
  # MH: Why is this not a parameter?
  variance <- 5

  # create random movement distance and angel in degree
  move_dist <-  stats::rnorm(n = nrow(fish_population),
                             mean = parameters$pop_mean_move, sd = sqrt(variance))

  # move towards reef
  if (reef_attraction & nrow(coords_reef) > 0) {

    # get coordinates within visibility left, straight in right of individuals
    heading_l <- cbind(fish_population$x +
                         (visibility * cos((fish_population$heading + -45) * (pi / 180))),
                       fish_population$y +
                         (visibility * sin((fish_population$heading + -45) * (pi / 180))))

    heading_s <- cbind(fish_population$x +
                         (visibility * cos(fish_population$heading * (pi / 180))),
                       fish_population$y +
                         (visibility * sin(fish_population$heading * (pi / 180))))

    heading_r <- cbind(fish_population$x +
                         (visibility * cos((fish_population$heading + 45) * (pi / 180))),
                       fish_population$y +
                         (visibility * sin((fish_population$heading + 45) * (pi / 180))))

    # torus translation if coords are outside plot
    heading_l <- int_translate_torus(coords = heading_l, extent = extent)

    heading_s <- int_translate_torus(coords = heading_s, extent = extent)

    heading_r <- int_translate_torus(coords = heading_r, extent = extent)

    # get distance values in directions
    # MH: This can be NA for individuals at the edge
    heading_l <- raster::extract(x = reef_dist, y = heading_l)

    heading_s <- raster::extract(x = reef_dist, y = heading_s)

    heading_r <- raster::extract(x = reef_dist, y = heading_r)

    # get ids of fish that turn one direction
    id_l <- which(heading_l < heading_s)

    id_r <- which(heading_r < heading_s)

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
  # MH: This could be correlated to heading before
  # MH: runif(min = heading - x, max = heading + x)
  fish_population$heading <- stats::runif(n = nrow(fish_population),
                                          min = 0, max = 360)

  # update activity
  # MH: Are there () missing?
  # MH: Why is + 1 added to mean move?
  # MH: Linear relationship between movement and activity not less than 1
  # MH: Should be basically higher if they swim more (ask Jake)
  fish_population$activity <-  (1 / (parameters$pop_mean_move + 1)) * move_dist + 1

  return(fish_population)
}
