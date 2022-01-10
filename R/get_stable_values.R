#' get_stable_values
#'
#' @description
#' Get values for stable biomass growth.
#'
#' @param bg_biomass,ag_biomass Numeric with starting bg and ag biomass.
#' @param parameters List with all model parameters.
#' @param verbose Logical if message should be printed.
#'
#' @details
#' Returns a list with starting values for i) the nutrients_pool and ii) the detritus_pool
#' which allow stable seagrass growth if no fish individuals are present. This means,
#' that both pools contain exactly the amount to balance the sloughed bg_biomass and ag_biomass
#' each  timestep.
#'
#' @return list
#'
#' @examples
#' get_stable_values(bg_biomass = arrR_starting_values$bg_biomass,
#' ag_biomass = arrR_starting_values$ag_biomass, parameters = arrR_parameters)
#'
#' @aliases get_stable_values
#' @rdname get_stable_values
#'
#' @export
get_stable_values <- function(bg_biomass, ag_biomass, parameters, verbose = TRUE) {

  # create input flag
  flag_input <- ifelse(test = parameters$nutrients_output > 0.0,
                       yes = TRUE, no = FALSE)

  # print message about nutrient input/output needed
  if (verbose) {

    if (flag_input) {

     message("> Returning nutrient input value because 'nutrients_output' > 0.")

    } else {

      message("> Returning no nutrient input value because 'nutrients_output' = 0.")

    }
  }

  # calculate detritus modifier for bg biomass
  bg_modf <- (bg_biomass - parameters$bg_biomass_min) /
    (parameters$bg_biomass_max - parameters$bg_biomass_min)

  # calculate detritus modifier for ag biomass
  ag_modf <- (ag_biomass - parameters$ag_biomass_min) /
    (parameters$ag_biomass_max - parameters$ag_biomass_min)

  # calculate ag detritus
  bg_detritus <- bg_biomass * (parameters$seagrass_slough * bg_modf)

  # calculate ag detritus
  ag_detritus <- ag_biomass * (parameters$seagrass_slough * ag_modf)

  # calculate amount of nutrients to keep ag and bg stable
  nutrients_pool <- (bg_detritus * parameters$bg_gamma) +
    (ag_detritus * parameters$ag_gamma)

  # calc output amount and set as input
  nutr_input <- nutrients_pool * parameters$nutrients_output

  # remove output amount from stable nutrients pool
  nutrients_pool <- nutrients_pool - nutr_input

  # calculate detritus amount for stable nutrients minus slough amount
  detritus_pool <- ((nutrients_pool + nutr_input) / parameters$detritus_mineralization) -
    ((bg_detritus * parameters$bg_gamma) + (ag_detritus * parameters$ag_gamma))

  # if detritus_mineralization is zero detritus pool will be Inf
  detritus_pool <- ifelse(test = is.infinite(detritus_pool),
                          yes = 0, no = detritus_pool)

  # if input is 0, return NULL
  if (!flag_input) {

    nutr_input <- NULL

  }

  # combine to result list
  result <- list(nutrients_pool = nutrients_pool, detritus_pool = detritus_pool,
                 nutr_input = nutr_input)

  return(result)
}
