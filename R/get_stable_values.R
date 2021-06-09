#' get_stable_values
#'
#' @description Get starting values for stable growth.
#'
#' @param starting_values List with all starting value parameters.
#' @param parameters List with all model parameters.
#'
#' @details
#' Returns a list with starting values which allow stable seagrass growth if no
#' fish individuals are present.
#'
#' @return list
#'
#' @examples
#' get_stable_values(starting_values = default_starting_values,
#' parameters = default_parameters)
#'
#' @aliases get_stable_values
#' @rdname get_stable_values
#'
#' @export
get_stable_values <- function(starting_values, parameters) {

  # calculate detritus modifier for bg biomass
  bg_modf <- (starting_values$bg_biomass - parameters$bg_biomass_min) /
    (parameters$bg_biomass_max - parameters$bg_biomass_min)

  # calculate detritus modifier for ag biomass
  ag_modf <- (starting_values$ag_biomass - parameters$ag_biomass_min) /
    (parameters$ag_biomass_max - parameters$ag_biomass_min)

  # calculate ag detritus
  bg_detritus <- starting_values$bg_biomass * (parameters$seagrass_slough * bg_modf)

  # calculate ag detritus
  ag_detritus <- starting_values$ag_biomass * (parameters$seagrass_slough * ag_modf)

  # calculate amount of nutrients to keep ag and bg stable
  nutrients_pool <- (bg_detritus * parameters$bg_gamma) +
    (ag_detritus * parameters$ag_gamma)

  # calculate detritus  amount for stable nutrients minus slough amount
  detritus_pool <- (nutrients_pool / parameters$detritus_mineralization) -
    ((ag_detritus * parameters$ag_gamma) + (bg_detritus * parameters$bg_gamma))

  # if detritus_mineralization is zero detritus pool will be Inf
  detritus_pool <- ifelse(test = is.infinite(detritus_pool),
                          yes = 0, no = detritus_pool)

  # combine to result list
  result <- list(nutrients_pool = nutrients_pool, detritus_pool = detritus_pool)

  return(result)
}
