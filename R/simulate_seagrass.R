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

  # calculate total nutrient uptake
  uptake_total_g <- int_calc_nutr_uptake(nutrients = seafloor_values$nutrients_pool,
                                         bg_biomass = seafloor_values$bg_biomass,
                                         ag_biomass = seafloor_values$ag_biomass,
                                         v_max = c(parameters$bg_v_max, parameters$ag_v_max),
                                         k_m = c(parameters$bg_k_m, parameters$ag_k_m),
                                         time_fac = min_per_i / 60)

  # get cell ids cells in which bg or ag growth
  id_bg_growth <- which(seafloor_values$bg_biomass < parameters$bg_biomass_max)

  id_ag_growth <- which(seafloor_values$bg_biomass >= parameters$bg_biomass_max &
                          seafloor_values$ag_biomass < parameters$ag_biomass_max)

  # below ground growth
  if (length(id_bg_growth) > 0) {

    # increase biomass
    seafloor_values$bg_biomass[id_bg_growth] <-
      seafloor_values$bg_biomass[id_bg_growth] + (uptake_total_g[id_bg_growth] / 0.0082)

    # remove nutrients used for growth from water column
    seafloor_values$nutrients_pool[id_bg_growth] <-
      seafloor_values$nutrients_pool[id_bg_growth] - uptake_total_g[id_bg_growth]

    # check which bg is above bg_max
    extra_bg <- which(seafloor_values$bg_biomass > parameters$bg_biomass_max)

    # reallocate nutrients to above ground
    if (length(extra_bg) > 0) {

      # add difference to ag
      seafloor_values$ag_biomass[extra_bg] <- seafloor_values$ag_biomass[extra_bg] +
        (seafloor_values$bg_biomass[extra_bg] - parameters$bg_biomass_max)

      # set bg to bg max
      seafloor_values$bg_biomass[extra_bg] <- parameters$bg_biomass_max

    }
  }

  # above ground growth
  if (length(id_ag_growth) > 0) {

    # increase biomass
    seafloor_values$ag_biomass[id_ag_growth] <-
      seafloor_values$ag_biomass[id_ag_growth] + (uptake_total_g[id_ag_growth] / 0.0144)

    # remove nutrients used for growth from water column
    seafloor_values$nutrients_pool[id_ag_growth] <-
      seafloor_values$nutrients_pool[id_ag_growth] - uptake_total_g[id_ag_growth]

  }

  # check which ag is above bg_max
  extra_ag <- which(seafloor_values$ag_biomass > parameters$ag_biomass_max)

  # set ag to ag max and release nutrients to detritus pool
  if (length(extra_ag) > 0) {

    # calculate nutrients of biomass > biomass max as detritus
    detritus <- (seafloor_values$ag_biomass[extra_ag] - parameters$ag_biomass_max) * 0.0144

    # add to detritus pool
    seafloor_values$detritus_pool[extra_ag] <- seafloor_values$detritus_pool[extra_ag] +
      detritus

    # set bg to bg max
    seafloor_values$ag_biomass[extra_ag] <- parameters$ag_biomass_max

  }

  # calculate biomass of detritus ratio
  # MH: If detritus is dynamic and detritus > growth, there will be biomass reduction
  detritus_bg <- seafloor_values$bg_biomass * parameters$detritus_ratio

  detritus_ag <- seafloor_values$ag_biomass * parameters$detritus_ratio

  # remove detritus from biomass
  seafloor_values$bg_biomass <- seafloor_values$bg_biomass - detritus_bg

  seafloor_values$ag_biomass <- seafloor_values$ag_biomass - detritus_ag

  # add nutrients to detritus pool
  seafloor_values$detritus_pool <- seafloor_values$detritus_pool +
    ((detritus_bg * 0.0082) + (detritus_ag * 0.0144))

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
