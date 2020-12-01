#' setup_fishpop
#'
#' @description Initiate fish population.
#'
#' @param seafloor Environment created with \code{\link{setup_seafloor}}.
#' @param starting_values List with all starting value parameters.
#' @param parameters List with all model parameters.
#' @param use_log Logical if TRUE, random log distribution is used.
#' @param verbose If TRUE, progress reports are printed.
#'
#' @details
#' Function to setup the fish population. If no fish shoud be created, set
#' \code{starting_values$pop_n = 0}.
#'
#' @return data.frame
#'
#' @examples
#' # Add example code
#'
#' @aliases setup_fishpop
#' @rdname setup_fishpop
#'
#' @export
setup_fishpop <- function(seafloor, starting_values, parameters, use_log = TRUE,
                                  verbose = TRUE) {

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

    heading <- stats::runif(n = n, min = 0, max = 360)

    # calculate length and weight
    size <- calc_size(pop_n = n,
                      pop_mean_size = parameters$pop_mean_size,
                      pop_max_size = parameters$pop_max_size,
                      pop_var_size = parameters$pop_var_size,
                      pop_a_grunt = parameters$pop_a_grunt,
                      pop_b_grunt = parameters$pop_b_grunt,
                      use_log = use_log)

    # calculate maximum reserves
    reserves_max <- parameters$pop_n_body * size$weight * parameters$pop_max_reserves

    # calculate starting reserves
    reserves <- parameters$pop_n_body * size$weight *
      parameters$pop_want_reserves # reserves_max

    # combine to final data frame
    fishpop <- data.frame(id = 1:n, age = 0,
                          x = x, y = y, heading = heading,
                          length = size$length, weight = size$weight,
                          reserves = reserves, reserves_max = reserves_max,
                          activity = numeric(n), respiration = numeric(n),
                          died_consumption = numeric(n), died_background = numeric(n))

  # No individuals need to be created
  } else {

    # combine to final data frame
    fishpop <- data.frame(id = numeric(), age = numeric(),
                          x = numeric(), y = numeric(), heading = numeric(),
                          length = numeric(), weight = numeric(),
                          reserves = numeric(), reserves_max = numeric(),
                          activity = numeric(), respiration = numeric(),
                          died_consumption = numeric(n), died_background = numeric(n))

  }

  return(fishpop)
}