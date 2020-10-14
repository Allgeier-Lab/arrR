#' simulate_seagrass
#'
#' @description Simulate seagrass.
#'
#' @param seafloor_values Data.frame with seafloor values.
#' @param parameters List with all model parameters.
#' @param cells_reef Vector with cell ids of AR.
#' @param min_per_i Integer to specify minutes per i.
#'
#' @details
#' Function to simulate processes of above ground and below ground seagrass.
#'
#' @references
#' DeAngelis, D.L., 1992. Dynamics of Nutrient Cycling and Food Webs. Springer
#' Netherlands, Dordrecht. https://doi.org/10.1007/978-94-011-2342-6
#'
#' @return RasterBrick
#'
#' @aliases simulate_seagrass
#' @rdname simulate_seagrass
#'
#' @export
simulate_seagrass <- function(seafloor_values, parameters, cells_reef, min_per_i) {

  # check if reef cells are available
  if (length(cells_reef) > 0) {

    # get current value of reef cells
    reef_ag <- seafloor_values$ag_biomass[cells_reef]
    reef_bg <- seafloor_values$bg_biomass[cells_reef]
    reef_nutr <- seafloor_values$nutrients_pool[cells_reef]
    reef_detritus <- seafloor_values$detritus_pool[cells_reef]
    reef_dead <- seafloor_values$detritus_dead[cells_reef]

  }

  # calculate total possible nutrient uptake
  uptake_total_g <- int_calc_nutr_uptake(nutrients = seafloor_values$nutrients_pool,
                                         bg_biomass = seafloor_values$bg_biomass,
                                         ag_biomass = seafloor_values$ag_biomass,
                                         v_max = c(parameters$bg_v_max, parameters$ag_v_max),
                                         k_m = c(parameters$bg_k_m, parameters$ag_k_m),
                                         time_fac = min_per_i / 60)

  # calculate bg growth in biomass; reduce growth closer to max biomass
  bg_growth <- (uptake_total_g / 0.0082) *
    ((parameters$bg_biomass_max - seafloor_values$bg_biomass) /
       parameters$bg_biomass_max)

    # increase bg biomass
  seafloor_values$bg_biomass <- seafloor_values$bg_biomass + bg_growth

  # remove nutrients used for bg growth from water column
  seafloor_values$nutrients_pool <- seafloor_values$nutrients_pool - (bg_growth * 0.0082)

  # remove nutrients from uptake
  uptake_total_g <- uptake_total_g - (bg_growth * 0.0082)

  id_ag_growth <- which(seafloor_values$bg_biomass >=
                          parameters$bg_biomass_max * parameters$bg_biomass_thres)

  # above ground growth
  if (length(id_ag_growth) > 0) {

    # calculate ag biomass grow; reduce growth closer to max biomass
    ag_growth <- (uptake_total_g[id_ag_growth] / 0.0144) *
      ((parameters$ag_biomass_max - seafloor_values$ag_biomass[id_ag_growth]) /
         parameters$ag_biomass_max)

    # increase biomass
    seafloor_values$ag_biomass[id_ag_growth] <-
      seafloor_values$ag_biomass[id_ag_growth] + ag_growth

    # remove nutrients used for growth from water column
    seafloor_values$nutrients_pool[id_ag_growth] <-
      seafloor_values$nutrients_pool[id_ag_growth] - (ag_growth * 0.0144)

  }

  # calculate biomass of detritus ratio
  bg_detritus <- (seafloor_values$bg_biomass - parameters$bg_biomass_min) *
    (parameters$detritus_ratio *
       ((-parameters$bg_biomass_min + seafloor_values$bg_biomass) /
          (-parameters$bg_biomass_min + parameters$bg_biomass_max)))

  ag_detritus <- (seafloor_values$ag_biomass - parameters$ag_biomass_min) *
    (parameters$detritus_ratio *
       ((-parameters$ag_biomass_min + seafloor_values$ag_biomass) /
          (-parameters$ag_biomass_min + parameters$ag_biomass_max)))

  # remove detritus from biomass
  seafloor_values$bg_biomass <- seafloor_values$bg_biomass - bg_detritus

  seafloor_values$ag_biomass <- seafloor_values$ag_biomass - ag_detritus

  # add nutrients to detritus pool
  seafloor_values$detritus_pool <- seafloor_values$detritus_pool +
    ((bg_detritus * 0.0082) + (ag_detritus * 0.0144))

  # check if reef cells are available
  if (length(cells_reef) > 0) {

    # set reef values to old values
    seafloor_values$ag_biomass[cells_reef] <- reef_ag
    seafloor_values$bg_biomass[cells_reef] <- reef_bg
    seafloor_values$nutrients_pool[cells_reef] <- reef_nutr
    seafloor_values$detritus_pool[cells_reef] <- reef_detritus
    seafloor_values$detritus_dead[cells_reef] <- reef_dead

  }

  return(seafloor_values)
}
