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

  # logical for final ok
  final_flag <- TRUE

  # specify all required starting values
  required_starting <- c("ag_biomass",
                         "bg_biomass",

                         "nutrients_pool",
                         "detritus_pool",

                         "pop_n",
                         "pop_mean_size",
                         "pop_var_size")

  # specify all required parameters
  required_parameters <- c("ag_biomass_max",
                           "ag_biomass_min",
                           "ag_v_max",
                           "ag_k_m",
                           "ag_gamma",

                           "bg_biomass_max",
                           "bg_biomass_min",
                           "bg_v_max",
                           "bg_k_m",
                           "bg_gamma",

                           "seagrass_thres",
                           "seagrass_slope",
                           "seagrass_slough",

                           "nutrients_diffusion",
                           "nutrients_output",

                           "detritus_mineralization",
                           "detritus_diffusion",
                           "detritus_fish_decomp",
                           "detritus_fish_diffusion",

                           "move_mean",
                           "move_var",
                           "move_visibility",
                           "move_border",
                           "move_reef",
                           "move_return",

                           "pop_max_reserves",
                           "pop_thres_reserves_min",
                           "pop_thres_reserves_max",
                           "pop_consumption_prop",

                           "pop_a",
                           "pop_b",
                           "pop_k",
                           "pop_linf",
                           "pop_n_body",

                           "resp_intercept",
                           "resp_slope",
                           "resp_temp_low",
                           "resp_temp_optm",
                           "resp_temp_max")

  # just print list with required parameters
  if (is.null(starting_values) && is.null(parameters)) {

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

  }

  # no starting values present, add NULL so later TRUE/FALSE is working
  else {

    check_starting <- NULL

    add_starting <- NULL

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

    # check if respiration temp is above max
    if (any(c(parameters$resp_temp_low, parameters$resp_temp_optm) >=
            parameters$resp_temp_max)) {

      # set final flag to false
      final_flag <- FALSE

      warning("'resp_temp_low' or 'resp_temp_optm' is >= 'resp_temp_max'.",
              call. = FALSE)

    }

    # check if min parameter is above maximum parameter
    if (any(c(parameters$bg_biomass_min, parameters$ag_biomass_min) >
            c(parameters$bg_biomass_max, parameters$ag_biomass_max))) {

      # set final flag to false
      final_flag <- FALSE

      warning("Minimum biomasses are larger than maximum biomasses.",
              call. = FALSE)

    }

    # pop_a must be positive
    if (parameters$pop_a < 0) {

      # set final flag to false
      final_flag <- FALSE

      warning("'pop_a' must be positive number.",
              call. = FALSE)

    }

    # check if all ratios are betwenn 0 and 1
    check_ratios <- any(c(c(parameters$seagrass_thres,
                            parameters$seagrass_slough,

                            parameters$nutrients_diffusion,
                            parameters$nutrients_output,

                            parameters$detritus_mineralization,
                            parameters$detritus_fish_decomp,
                            parameters$detritus_diffusion,
                            parameters$detritus_fish_diffusion,

                            parameters$pop_max_reserves) > 1,

                          c(parameters$seagrass_slough,

                            parameters$nutrients_diffusion,
                            parameters$nutrients_output,

                            parameters$detritus_mineralization,
                            parameters$detritus_fish_decomp,
                            parameters$detritus_diffusion,
                            parameters$detritus_fish_diffusion,

                            parameters$pop_max_reserves) < 0))

    # check if all fraction are between 0 and 1
    if (check_ratios) {

      final_flag <- FALSE

      warning("Some parameters that must be 0 <= x <= 1 are outside range.",
              call. = FALSE)

    }
  }

  # no parameter values present, add NULL so later TRUE/FALSE is working
  else {

    check_parameters <- NULL

    add_parameters <- NULL

  }

  # check if some parameters make sense
  if (!is.null(starting_values) && !is.null(parameters)) {

    if (verbose) {

      message("> ...Checking if starting values are within parameter boundaries...\n")

    }

    # check if biomass starting is above max
    if (any(c(starting_values$bg_biomass, starting_values$ag_biomass) >
            c(parameters$bg_biomass_max, parameters$ag_biomass_max))) {

      # set final flag to false
      final_flag <- FALSE

      warning("Starting biomasses are larger than maximum biomasses.", call. = FALSE)

    }

    # check if biomass starting is below min
    if (any(c(starting_values$bg_biomass, starting_values$ag_biomass) <
            c(parameters$bg_biomass_min, parameters$ag_biomass_min))) {

      # set final flag to false
      final_flag <- FALSE

      warning("Starting biomasses are smaller than minimum biomasses.",
              call. = FALSE)

    }
  }

  # check if additional values are present
  if (length(add_starting) > 0) {

    # combine additional values with separator
    additional_starting <- paste(names(starting_values)[add_starting],
                                 collapse = " ")

    # return additional starting values
    # return additional parameter
    if (verbose) {

      message("> Not needed starting values: ",
              additional_starting, "\n")

    }
  }

  # check if additional values are present
  if (length(add_parameters) > 0 ) {

    # combine additional values with separator
    additional_parameters <- paste(names(parameters)[add_parameters],
                                   collapse = " ")

    # return additional parameter
    if (verbose) {

      message("> Not needed parameter values: ",
              additional_parameters)

    }
  }

  # check if any values are missing
  if (length(check_starting) > 0) {

    # combine missing values with separator
    missing_starting <- paste(required_starting[check_starting], collapse = " ")

    # set final flag to false
    final_flag <- FALSE

    warning("Missing starting values: ", missing_starting,
            call. = FALSE)

  }

  # check if parameters are missing
  if (length(check_parameters) > 0) {

    # combine missing values with separator
    missing_parameters <- paste(required_parameters[check_parameters],
                                collapse = " ")

    # set final flag to false
    final_flag <- FALSE

    warning("Missing parameter values: ", missing_parameters,
            call. = FALSE)

  }

  # print final message
  if (verbose && final_flag) {

    message("\n> All checking done!")

  }

  else if (verbose && !final_flag) {

    message("\n> Make sure to check critical warnings!")

  }
}
