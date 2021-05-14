#' simulate_seagrass
#'
#' @description Simulate seagrass.
#'
#' @param seafloor_values Matrix with seafloor values.
#' @param parameters List with all model parameters.
#' @param cells_reef Vector with cell ids of AR.
#' @param time_frac Integer to scale uptake from hour to min_per_i times seagrass_each.
#'
#' @details
#' Function to simulate processes of aboveground and belowground seagrass growth and
#' slough.
#'
#' @references
#' DeAngelis, D.L., 1992. Dynamics of Nutrient Cycling and Food Webs. Springer
#' Netherlands, Dordrecht. https://doi.org/10.1007/978-94-011-2342-6
#'
#' @return Matrix
#'
#' @aliases simulate_seagrass
#' @rdname simulate_seagrass
#'
#' @export
simulate_seagrass <- function(seafloor_values, parameters, cells_reef, time_frac) {

  rcpp_seagrass_growth(seafloor = seafloor_values,
                       cells_reef = cells_reef,
                       bg_v_max = parameters$bg_v_max, bg_k_m = parameters$bg_k_m, bg_gamma = parameters$bg_gamma,
                       ag_v_max = parameters$ag_v_max, ag_k_m = parameters$ag_k_m, ag_gamma = parameters$ag_gamma,
                       bg_biomass_max = parameters$bg_biomass_max, bg_biomass_min = parameters$bg_biomass_min,
                       ag_biomass_max = parameters$ag_biomass_max, ag_biomass_min = parameters$ag_biomass_min,
                       seagrass_thres = parameters$seagrass_thres, seagrass_slope = parameters$seagrass_slope,
                       detritus_ratio = parameters$detritus_ratio,
                       time_frac = time_frac)

}
