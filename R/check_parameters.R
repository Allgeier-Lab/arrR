#' check_parameters
#'
#' @description Check model parameters and starting values.
#'
#' @param starting_values List with all starting value parameters.
#' @param parameters List with all model parameters.
#' @param verbose If TRUE, progress reports are printed.
#'
#' @details
#' The function checks if all required starting values and parameters are provided.
#' If values are missing, a vector with the name of the corresponding values will
#' be returned. If no arguments are provided, a list of all required values is printed
#'
#' @return void
#'
#' @examples
#' # Add example code
#'
#' @aliases check_parameters
#' @rdname check_parameters
#'
#' @export
check_parameters <- function(starting_values = NULL, parameters = NULL, verbose = TRUE) {

  # specify all required starting values
  required_starting <- c("ag_biomass",
                         "bg_biomass",
                         "nutrients_pool",
                         "detritus_pool",
                         "pop_n",
                         "water_temp")

  # specify all required parameters
  required_parameters <- c("ag_biomass_max",
                           "ag_biomass_min",
                           "ag_v_max",
                           "ag_k_m",
                           "ag_sigmoid_slope",
                           "ag_reduction",
                           "bg_biomass_max",
                           "bg_biomass_min",
                           "bg_v_max",
                           "bg_k_m",
                           "bg_sigmoid_slope",
                           "bg_reduction",
                           "nutrients_diffusion",
                           "detritus_ratio",
                           "detritus_mineralization",
                           "detritus_diffusion",
                           "detritus_dead_diffusion",
                           "detritus_dead_decomp",
                           "pop_mean_size",
                           "pop_var_size",
                           "pop_max_size",
                           "pop_max_reserves",
                           "pop_want_reserves",
                           "pop_visibility",
                           "pop_mean_move",
                           "pop_var_move",
                           "pop_a_grunt",
                           "pop_b_grunt",
                           "pop_k_grunt",
                           "pop_linf_grunt",
                           "pop_n_body",
                           "resp_intercept",
                           "resp_slope",
                           "resp_temp_low",
                           "resp_temp_optm",
                           "resp_temp_max")

  # just print list with required parameters
  if (is.null(starting_values) & is.null(parameters)) {

    if (verbose) {

      # return warning
      message("> Required starting values:\n", paste(required_starting, collapse = "\n"))

      message("\n")

      message("> Required parameters:\n", paste(required_parameters, collapse = "\n"))
    }
  }

  # check starting values only
  if (!is.null(starting_values)) {

    if (verbose) {

      message("> ...Checking starting values...")

    }

    # check if all starting values are present
    check_starting <- which(!required_starting %in% names(starting_values))

    # check if any additional parameters are present
    add_starting <- which(!names(starting_values) %in% required_starting)

    # check if any values are missing
    if (length(check_starting) > 0) {

      # combine missing values with separator
      missing_starting <- paste(required_starting[check_starting], collapse = " ")

      warning("The following starting values are missing: ", missing_starting,
              call. = FALSE)

    }

    # check if additional values are present
    if (length(add_starting) > 0 ) {

      # combine additional values with separator
      additional_starting <- paste(names(starting_values)[add_starting],
                                   collapse = " ")

      # return additional starting values
      # return additional parameter
      if (verbose) {

        message("> The following starting values are not needed: ",
                additional_starting, "\n")

      }
    }
  }

  # check parameters only
  if (!is.null(parameters)) {

    if (verbose) {

      message("> ...Checking parameter values...")

    }

    # check if all parameters are there
    check_parameters <- which(!required_parameters %in% names(parameters))

    # check if additional parameters are present
    add_parameters <- which(!names(parameters) %in% required_parameters)

    # check if parameters are missing
    if (length(check_parameters) > 0) {

      # combine missing values with separator
      missing_parameters <- paste(required_parameters[check_parameters],
                                  collapse = " ")

      warning("The following parameter values are missing: ", missing_parameters,
              call. = FALSE)

    }

    # check if additional values are present
    if (length(add_parameters) > 0 ) {

      # combine additional values with separator
      additional_parameters <- paste(names(parameters)[add_parameters],
                                     collapse = " ")

      # return additional parameter
      if (verbose) {

        message("> The following parameter values are not needed: ",
                additional_parameters, "\n")

      }
    }

    # check if respiration temp is above max
    if (any(c(parameters$resp_temp_low, parameters$resp_temp_optm) >=
            parameters$resp_temp_max)) {

      warning("'resp_temp_low' or 'resp_temp_optm' is >= 'resp_temp_max'.",
              call. = FALSE)

    }
  }

  # check if some parameters make sense
  if (!is.null(starting_values) & !is.null(parameters)) {

    if (verbose) {

      message("> ...Checking if starting values are within parameter boundaries...")

    }

    # check if biomass starting is above max
    if (any(c(starting_values$bg_biomass, starting_values$ag_biomass) >
            c(parameters$bg_biomass_max, parameters$ag_biomass_max))) {

      warning("Starting biomasses are larger than maximum biomasses.", call. = FALSE)
    }

    # check if biomass starting is below min
    if (any(c(starting_values$bg_biomass, starting_values$ag_biomass) <
            c(parameters$bg_biomass_min, parameters$ag_biomass_min))) {

      warning("Starting biomasses are smaller than minimum biomasses.",
              call. = FALSE)

    }

    # check if min parameter is above maximum parameter
    if (any(c(parameters$bg_biomass_min, parameters$ag_biomass_min) >
            c(parameters$bg_biomass_max, parameters$ag_biomass_max))) {

      warning("Minimum biomasses are larger than maximum biomasses.",
              call. = FALSE)
    }
  }

  if (verbose) {

    message("\n> All checking done.")

  }
}
