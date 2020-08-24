#' setup_population
#'
#' @description Initiate fish population.
#'
#' @param n Integer with number of individuals.
#' @param environment Environment created with \code{\link{setup_environment}}.
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
#' @return data frame
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
#' input_environment <- setup_environment(extent = c(100, 100), grain = 1,
#' reefs = reef_matrix, starting_values = starting_values, parameters = parameters)
#'
#' input_population <- setup_population(n = 50, environment = input_environment,
#' starting_values = starting_values, parameters = parameters)
#'
#' @aliases setup_population
#' @rdname setup_population
#'
#' @export
setup_population <- function(n, environment, starting_values, parameters, verbose = TRUE) {

  # create random coordinates within environment
  x <- stats::runif(n = starting_values$n, min = raster::xmin(environment),
                    max = raster::xmax(environment))

  y <- stats::runif(n = starting_values$n, min = raster::ymin(environment),
                    max = raster::ymax(environment))

  # calculate size and weight
  size <- int_calc_size(n = n, parameters = parameters)

  # what is this?
  n_body <- 2.999
  aen <-  0.75

  # where do these numbers/parameters come from?
  # why is reserves and reserves_max identical?
  reserves_max <- n_body / 100 * size$weight * 0.05
  reserves <- n_body / 100 * size$weight * 0.05   ;

  # combine to final data frame
  population <- data.frame(i = 0, id = 1:n, x = x, y = y,
                           size = size$size, weight = size$weight,
                           aen = aen, n_body = n_body,
                           reserves = reserves, reserves_max = reserves_max)

  if (verbose) {

    message("Created ", n, " individuals within ", raster::extent(environment), "...")
  }

  return(population)
}
