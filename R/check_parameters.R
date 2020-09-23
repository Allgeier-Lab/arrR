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
                         "wc_nutrients",
                         "pop_n",
                         "water_temp")

  # specify all required parameters
  required_parameters <- c("ag_biomass_thres",
                           "ag_biomass_max",
                           "ag_v_max",
                           "ag_k_max",
                           "ag_sigmoid_slope",
                           "ag_reduction",
                           "ag_slough_ratio",
                           "bg_biomass_max",
                           "bg_v_max",
                           "bg_k_max",
                           "bg_sigmoid_slope",
                           "bg_reduction",
                           "bg_slough_ratio",
                           "slough_detritus_ratio",
                           "detritus_fraction",
                           "detritus_diffusion",
                           "detritus_death_diffusion",
                           "detritus_death_decomp",
                           "wc_diffusion",
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

  # check if all starting values are present
  check_starting <- which(!required_starting %in% names(starting_values))

  # check if all parameters are there
  check_parameters <- which(!required_parameters %in% names(parameters))

  # length equals 0 if all parameters are present
  if (length(check_starting) == 0 & length(check_parameters) == 0 ) {

    message("> All starting values and parameters are available...")

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
