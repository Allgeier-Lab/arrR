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

  # convert water column nutrients to umol/l
  nutrients_pool_umol <- int_convert_nutr(seafloor_values$nutrients_pool,
                                        to = "umol") / 10000

  # calculate bg and ag uptake depending on nutrients and biomass
  # convert uptake parameters to correct tick scale (from per h to day)
  uptake_bg_umol <- int_calc_uptake(nutrients = nutrients_pool_umol,
                                    biomass = seafloor_values$bg_biomass,
                                    v_max = parameters$bg_v_max / 60 * min_per_i, # * 24
                                    k_m = parameters$bg_k_m)

  uptake_ag_umol <- int_calc_uptake(nutrients = nutrients_pool_umol,
                                    biomass = seafloor_values$ag_biomass,
                                    v_max = parameters$ag_v_max / 60 * min_per_i, # * 24
                                    k_m = parameters$ag_k_m)

  # sum bg and ag to get total uptake in g
  uptake_total_g <- int_convert_nutr(x = uptake_bg_umol + uptake_ag_umol, to = "g")

  # check if total uptake exceeds total available nutrients
  uptake_total_g <- ifelse(test = uptake_total_g > seafloor_values$nutrients_pool,
                           yes = seafloor_values$nutrients_pool, no = uptake_total_g)

  # get cell ids cells in which bg or ag growth
  id_bg_growth <- which(seafloor_values$bg_biomass < parameters$bg_biomass_max)

  id_ag_growth <- which(seafloor_values$bg_biomass >= parameters$bg_biomass_max &
                          seafloor_values$ag_biomass < parameters$ag_biomass_thres)

  # below ground growth
  if (length(id_bg_growth) > 0) {

    # calculation growing values
    growth_temp <- int_seagrass_growth(nutrients = uptake_total_g[id_bg_growth],
                                       gamma = 0.0082,
                                       detritus_ratio = parameters$detritus_ratio,
                                       detritus_decomposition = parameters$detritus_decomposition)

    # increase biomass
    seafloor_values$bg_biomass[id_bg_growth] <-
      seafloor_values$bg_biomass[id_bg_growth] + growth_temp$biomass

    # check which bg is above bg_max
    extra_bg <- which(seafloor_values$bg_biomass > parameters$bg_biomass_max)

    # reallocate nutrients to above ground
    if (length(extra_bg) > 0) {

      # add difference to ag
      seafloor_values$ag_biomass[extra_bg] <- seafloor_values$ag_biomass[extra_bg] +
        (seafloor_values$bg_biomass[extra_bg] - parameters$bg_biomass_max)

      # set bg to bg max
      seafloor_values$bg_biomass[extra_bg] <-  parameters$bg_biomass_max

    }

    # remove nutrients used for growth from water column
    seafloor_values$nutrients_pool[id_bg_growth] <-
      seafloor_values$nutrients_pool[id_bg_growth] - growth_temp$nutrients

    # increase detritus pool
    seafloor_values$detritus_pool[id_bg_growth] <-
      seafloor_values$detritus_pool[id_bg_growth] + growth_temp$detritus

  }

  # above ground growth
  if (length(id_ag_growth) > 0) {

    # calculation growing values
    growth_temp <- int_seagrass_growth(nutrients = uptake_total_g[id_ag_growth],
                                       gamma = 0.0144,
                                       detritus_ratio = parameters$detritus_ratio,
                                       detritus_decomposition = parameters$detritus_decomposition)

    # increase biomass
    seafloor_values$ag_biomass[id_ag_growth] <-
      seafloor_values$ag_biomass[id_ag_growth] + growth_temp$biomass

    # remove nutrients used for growth from water column
    seafloor_values$nutrients_pool[id_ag_growth] <-
      seafloor_values$nutrients_pool[id_ag_growth] - growth_temp$nutrients

    # increase detritus pool
    seafloor_values$detritus_pool[id_ag_growth] <-
      seafloor_values$detritus_pool[id_ag_growth] + growth_temp$detritus

  }

  # check which ag is above bg_max
  extra_ag <- which(seafloor_values$ag_biomass > parameters$ag_biomass_max)

  # set ag to ag max and release nutrients to detritius pool
  if (length(extra_ag) > 0) {

    # add difference to detritius pool 0.0144
    seafloor_values$detritus_pool[extra_ag] <- seafloor_values$detritus_pool[extra_ag] +
        ((seafloor_values$ag_biomass[extra_ag] - parameters$ag_biomass_max) * 0.0144)

    # set bg to bg max
    seafloor_values$ag_biomass[extra_ag] <- parameters$ag_biomass_max

  }

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
