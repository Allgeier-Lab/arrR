#' check_parameters
#'
#' @description Check model parameters
#'
#' @param starting_values List with all starting value parameters.
#' @param parameters List with all model parameters.
#'
#' @details
#' The function checks if all required starting values and parameters are provided.
#' If values are missing, a vector with the name of the corresponding values will
#' be returned.
#'
#' @return vector
#'
#' @examples
#' starting_values <- system.file("extdata", "starting_values.csv", package = "coRal")
#' parameters <- system.file("extdata", "parameters.csv", package = "coRal")
#'
#' starting_values <- read_parameters(file = starting_values, sep = ";")
#' parameters <- read_parameters(file = parameters, sep = ";")
#'
#' check_parameters(starting_values = starting_values, parameters = parameters)
#'
#' @aliases check_parameters
#' @rdname check_parameters
#'
#' @export
check_parameters <- function(starting_values, parameters) {

  # specify all required starting values
  required_starting <- c("ag_biomass",
                         "bg_biomass",
                         "n")

  # check if all starting values are present
  check_starting <- which(!required_starting %in% names(starting_values))


  # specify all required parameters
  required_parameters <- c("sg_density",
                           "gamma_ag",
                           "gamma_bg",
                           "detrital_fraction",
                           "wc_nutrients",
                           "mean_size",
                           "a_grunt",
                           "b_grunt")

  # check if all parameters are there
  check_parameters <- which(!required_parameters %in% names(parameters))

  # length equals 0 if all parameters are present
  if (length(check_starting) == 0 & length(check_parameters) == 0 ) {

    message("All starting values and parameters are available...")

  } else {

    # combine missing values with separator
    missing <- paste(required_starting[check_starting],
                     required_parameters[check_parameters],
                     sep = " ")

    # return warning
    warning("The following parameters are missing: ", missing,
            call. = FALSE)

  }
}
