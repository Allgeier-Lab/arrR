#' setup_fishpop
#'
#' @description
#' Setup fish population for model run.
#'
#' @param seafloor SpatRaster object.
#' @param starting_values List with all starting value parameters.
#' @param parameters List with all model parameters.
#' @param use_log Logical if TRUE, random log distribution is used.
#' @param verbose Logical if TRUE, information is printed.
#'
#' @details
#' Function to setup the fish population. If \code{use_log = TRUE} the size distribution
#' of the fish population follows a log-norm distribution. For more information, see
#' \code{calc_size} (internal function). To create no fish, set \code{starting_values$pop_n = 0}.
#'
#' @return data.frame
#'
#' @examples
#' reef <- matrix(data = c(-1, 0, 0, 1, 1, 0, 0, -1, 0, 0),
#' ncol = 2, byrow = TRUE)
#'
#' seafloor <- setup_seafloor(dimensions = c(100, 100), grain = 1,
#' reef = reef, starting_values = default_starting)
#' fishpop <- setup_fishpop(seafloor = seafloor,
#' starting_values = default_starting, parameters = default_parameters)
#'
#' @aliases setup_fishpop
#' @rdname setup_fishpop
#'
#' @export
setup_fishpop <- function(seafloor, starting_values, parameters, use_log = TRUE,
                          verbose = TRUE) {

  if (verbose) {

    message("> ...Creating ", starting_values$pop_n, " individuals within ",
            terra::ext(seafloor), "...")

  }

  # create fishpop
  if (starting_values$pop_n != 0) {

    # create random coordinates within environment
    x <- stats::runif(n = starting_values$pop_n, min = terra::xmin(seafloor),
                      max = terra::xmax(seafloor))

    y <- stats::runif(n = starting_values$pop_n, min = terra::ymin(seafloor),
                      max = terra::ymax(seafloor))

    heading <- stats::runif(n = starting_values$pop_n, min = 0, max = 360)

    # calculate length and weight
    size <- calc_size(pop_n = starting_values$pop_n,
                      pop_mean_size = starting_values$pop_mean_size,
                      pop_linf = parameters$pop_linf,
                      pop_var_size = starting_values$pop_var_size,
                      pop_a = parameters$pop_a, pop_b = parameters$pop_b,
                      use_log = use_log)

    # calculate maximum reserves
    reserves_max <- parameters$pop_n_body * size$weight * parameters$pop_reserves_max

    # create starting reserves
    reserves <- stats::runif(n = starting_values$pop_n,
                             min = reserves_max * 0.5, max = reserves_max)

    # combine to final data frame
    fishpop <- data.frame(id = 1:starting_values$pop_n, age = 0.0,
                          x = x, y = y, heading = heading,
                          length = size$length, weight = size$weight,
                          activity = numeric(starting_values$pop_n),
                          respiration = numeric(starting_values$pop_n),
                          reserves = reserves, reserves_max = reserves_max,
                          behavior = rep(x = 3.0, times = starting_values$pop_n),
                          consumption = numeric(starting_values$pop_n),
                          excretion = numeric(starting_values$pop_n),
                          died_consumption = numeric(starting_values$pop_n),
                          died_background = numeric(starting_values$pop_n))

  # No individuals need to be created
  } else {

    # combine to final data frame
    fishpop <- data.frame(id = numeric(), age = numeric(),
                          x = numeric(), y = numeric(), heading = numeric(),
                          length = numeric(), weight = numeric(),
                          activity = numeric(), respiration = numeric(),
                          reserves = numeric(), reserves_max = numeric(),
                          behavior = numeric(), consumption = numeric(), excretion = numeric(),
                          died_consumption = numeric(), died_background = numeric())

  }

  return(fishpop)
}
