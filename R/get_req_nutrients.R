#' get_req_nutrients
#'
#' @description
#' Get required nutrients for stable biomass growth.
#'
#' @param bg_biomass,ag_biomass Numeric with starting bg and ag biomass.
#' @param parameters List with all model parameters.
#'
#' @details
#' Returns a list with starting values for i) the nutrients_pool and ii) the detritus_pool,
#' which allow stable seagrass growth if no fish individuals are present.
#'
#' This means that both pools contain exactly the amount to balance the sloughed
#' bg_biomass and ag_biomass each time step.
#'
#' @return list
#'
#' @examples
#' get_req_nutrients(bg_biomass = default_starting$bg_biomass,
#' ag_biomass = default_starting$ag_biomass, parameters = default_parameters)
#'
#' @aliases get_req_nutrients
#' @rdname get_req_nutrients
#'
#' @export
get_req_nutrients <- function(bg_biomass, ag_biomass, parameters) {

  # calculate detritus modifier for ag biomass
  ag_modf <- (ag_biomass - parameters$ag_biomass_min) /
    (parameters$ag_biomass_max - parameters$ag_biomass_min)

  # calculate ag detritus
  ag_detritus <- ag_biomass * (parameters$seagrass_slough * ag_modf)

  # calculate detritus modifier for bg biomass
  bg_modf <- (bg_biomass - parameters$bg_biomass_min) /
    (parameters$bg_biomass_max - parameters$bg_biomass_min)

  # calculate bg detritus
  bg_detritus <- bg_biomass * (parameters$seagrass_slough * bg_modf)

  # calculate amount of nutrients to keep ag and bg stable
  nutrients_required <- (ag_detritus * parameters$ag_gamma) +
    (bg_detritus * parameters$bg_gamma)

  # calculate detritus amount for stable nutrients minus slough amount
  detritus_pool <- ((nutrients_required / parameters$detritus_mineralization) -
    nutrients_required)

  # combine to result list
  result <- list(nutrients_pool = nutrients_required, detritus_pool = detritus_pool)

  return(result)
}
