#' setup_fishpop
#'
#' @description
#' Setup fish population for model run.
#'
#' @param seafloor Data.frame object.
#' @param species Vector with species id.
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
#' The species id vector must have the same length as \code{starting_values$pop_n} and contain
#' species ids between 1 and i.
#'
#' @return data.frame
#'
#' @examples
#' reef <- matrix(data = c(-1, 0, 0, 1, 1, 0, 0, -1, 0, 0),
#' ncol = 2, byrow = TRUE)
#'
#' seafloor <- setup_seafloor(dimensions = c(50, 50), grain = 1,
#' reef = reef, starting_values = default_starting)
#' fishpop <- setup_fishpop(seafloor = seafloor,
#' starting_values = default_starting, parameters = default_parameters)
#'
#' @aliases setup_fishpop
#' @rdname setup_fishpop
#'
#' @export
setup_fishpop <- function(seafloor, species, starting_values, parameters,
                          use_log = TRUE, verbose = TRUE) {

  # get seafloor info
  seafloor_info <- get_seafloor_dim(seafloor)

  if (verbose) {

    message("> ...Creating ", starting_values$pop_n, " individuals within ",
            paste(seafloor_info$extent, collapse = " "), " (xmin, xmax, ymin, max)...")

  }

  # create fishpop
  if (starting_values$pop_n != 0) {

    # check if species vector has correct size
    if (starting_values$pop_n != length(species)) {

      stop("Species vector must be same length as pop_n", call. = FALSE)

    }

    # check if starting values are correct for species
    # SR: now allows there to be one species present even if two species are coded
    if (length(starting_values$pop_mean_size) < length(unique(species)) |
        length(starting_values$pop_sd_size) < length(unique(species))) {

      stop("'pop_mean_size' and 'pop_mean_sd' must store a value for each species", call. = FALSE)

    }

    # check params
    check_param <- any(vapply(c("move_", "pop_", "resp_"), function(x) {

      any(vapply(parameters[startsWith(x = names(parameters), prefix = x)], function(y) {
#SR: same thing here, number of unique species present in model does not have to be equal to
# parameters available, but if there are more unique species present than parameters, this is bad
        length(y) < length(unique(species))

      }, FUN.VALUE = logical(1)))
    }, FUN.VALUE = logical(1)))

    if (check_param) {

      stop("All move_*, pop_*, or resp_* must store a value for each species.", call. = FALSE)

    }

    seafloor_info <- get_seafloor_dim(seafloor)

    # create random coordinates within environment
    x <- stats::runif(n = starting_values$pop_n, min = seafloor_info$extent[[1]],
                      max = seafloor_info$extent[[2]])

    y <- stats::runif(n = starting_values$pop_n, min = seafloor_info$extent[[3]],
                      max = seafloor_info$extent[[4]])

    heading <- stats::runif(n = starting_values$pop_n, min = 0, max = 360)

    # calculate length and weight
    # use pop_ldie values to ensure all fish start within constraints
    size <- calc_size(pop_n = starting_values$pop_n,
                      pop_mean_size = starting_values$pop_mean_size[species],
                      pop_linf = parameters$pop_ldie[species],
                      pop_sd_size = starting_values$pop_sd_size[species],
                      pop_a = parameters$pop_a[species], pop_b = parameters$pop_b[species],
                      use_log = use_log)

    # calculate maximum reserves
    reserves_max <- parameters$pop_n_body[species] * size$weight * parameters$pop_reserves_max[species]

    # create starting reserves
    reserves <- stats::runif(n = starting_values$pop_n,
                             min = reserves_max * 0.5, max = reserves_max)

    # combine to final data frame
    fishpop <- data.frame(id = 1:starting_values$pop_n, species = species, age = 0.0,
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
    fishpop <- data.frame(id = numeric(), species = numeric(), age = numeric(),
                          x = numeric(), y = numeric(), heading = numeric(),
                          length = numeric(), weight = numeric(),
                          activity = numeric(), respiration = numeric(),
                          reserves = numeric(), reserves_max = numeric(),
                          behavior = numeric(), consumption = numeric(), excretion = numeric(),
                          died_consumption = numeric(), died_background = numeric())

  }

  return(fishpop)
}
