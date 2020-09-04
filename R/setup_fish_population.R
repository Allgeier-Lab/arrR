#' setup_fish_population
#'
#' @description Initiate fish population.
#'
#' @param seafloor Environment created with \code{\link{setup_seafloor}}.
#' @param starting_values List with all starting value parameters.
#' @param parameters List with all model parameters.
#' @param verbose If TRUE, progress reports are printed.
#'
#' @details
#' Function to setup the environment.....
#' Center of the environment is always set to (0,0).
#'
#' Parameters include ...
#'
#' @return data.table
#'
#' @examples
#' reef_matrix <- matrix(data = c(-1, 0, 0, 1, 1, 0, 0, -1, 0, 0),
#' ncol = 2, byrow = TRUE)
#'
#' starting_values <- system.file("extdata", "starting_values.csv", package = "coRal")
#' parameters <- system.file("extdata", "parameters.csv", package = "coRal")
#' starting_values <- read_parameters(file = starting_values, sep = ";")
#' parameters <- read_parameters(file = parameters, sep = ";")
#'
#' input_seafloor <- setup_seafloor(extent = c(100, 100), grain = 1,
#' reefs = reef_matrix, starting_values = starting_values, parameters = parameters)
#'
#' input_fish_population <- setup_fish_population(seafloor = input_seafloor,
#' starting_values = starting_values, parameters = parameters)
#'
#' @aliases setup_fish_population
#' @rdname setup_fish_population
#'
#' @export
setup_fish_population <- function(seafloor, starting_values, parameters, verbose = TRUE) {

  n <- starting_values$pop_n

  if (verbose) {

    message("> Creating ", n, " individuals within ", raster::extent(seafloor), "...")

  }

  #
  if (n != 0) {

    # create random coordinates within environment
    x <- stats::runif(n = n, min = raster::xmin(seafloor),
                      max = raster::xmax(seafloor))

    y <- stats::runif(n = n, min = raster::ymin(seafloor),
                      max = raster::ymax(seafloor))

    # calculate length and weight
    size <- int_calc_size(starting_values = starting_values,
                          parameters = parameters)

    # MH: What is this?
    # MH: Why are this values not treated as parameters?
    # MH: aen=N assimilation efficiency?
    # MH: But if its identical all the time, no need to store it for each individual
    n_body <- 2.999
    aen <-  0.75

    # MH: Where do these formula come from?
    # MH: Why is reserves_max = reserves?
    reserves_max <- n_body / 100 * size$weight * 0.05
    reserves <- n_body / 100 * size$weight * 0.05

    # combine to final data frame
    fish_population <- data.frame(id = 1:n, age = 0,
                                  x = x, y = y,
                                  length = size$length, weight = size$weight,
                                  aen = aen, n_body = n_body,
                                  reserves = reserves, reserves_max = reserves_max,
                                  reserves_diff = reserves_max - reserves,
                                  activity = numeric(n), respiration = numeric(n),
                                  growth_length = numeric(n), growth_weight = numeric(n),
                                  growth_nutrient = numeric(n), consumption_req = numeric(n),
                                  died_consumption = numeric(n), died_background = numeric(n))

  # No individuals need to be created
  } else {

    # combine to final data frame
    fish_population <- data.frame(id = numeric(), age = numeric(),
                                  x = numeric(), y = numeric(),
                                  length = numeric(), weight = numeric(),
                                  aen = numeric(), n_body = numeric(),
                                  reserves = numeric(), reserves_max = numeric(),
                                  reserves_diff = numeric(),
                                  activity = numeric(), respiration = numeric(),
                                  growth_length = numeric(), growth_weight = numeric(),
                                  growth_nutrient = numeric(),
                                  consumption_req = numeric(),
                                  died_consumption = numeric(n), died_background = numeric(n))
  }

  return(fish_population)
}
