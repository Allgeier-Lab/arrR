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
    reef_detritus <- seafloor_values$detritus_pool[cells_reef]
    reef_dead <- seafloor_values$detritus_dead[cells_reef]
    reef_nutr <- seafloor_values$wc_nutrients[cells_reef]

  }

  # convert water column nutrients to umol/l
  wc_nutrients_umol <- int_convert_nutr(seafloor_values$wc_nutrients,
                                        to = "umol") / 10000

  # calculate bg and ag uptake depending on nutrients and biomass
  # convert uptake parameters to correct tick scale (from per h to day)
  # MH: Formula 3.2; Formula 3.7;
  uptake_bg_umol <- int_calc_uptake(nutrients = wc_nutrients_umol,
                                    biomass = seafloor_values$bg_biomass,
                                    v_max = parameters$bg_v_max / 60 * min_per_i, # * 24
                                    k_m = parameters$bg_k_m)

  uptake_ag_umol <-  int_calc_uptake(nutrients = wc_nutrients_umol,
                                     biomass = seafloor_values$ag_biomass,
                                     v_max = parameters$ag_v_max / 60 * min_per_i, # * 24
                                     k_m = parameters$ag_k_m)

  # sum bg and ag to get total uptake in g
  uptake_total_g <- int_convert_nutr(x = uptake_bg_umol + uptake_ag_umol, to = "g")

  # check if total uptake exceeds total available nutrients
  uptake_total_g <- ifelse(test = uptake_total_g > seafloor_values$wc_nutrients,
                           yes = seafloor_values$wc_nutrients, no = uptake_total_g)

  # get cell ids cells in which bg or ag growth
  id_bg_growth <- which(seafloor_values$bg_biomass < parameters$bg_biomass_max)

  id_ag_growth <- which(seafloor_values$bg_biomass >= parameters$bg_biomass_max &
                          seafloor_values$ag_biomass < parameters$ag_biomass_thres)

  # below ground growth
  if (length(id_bg_growth) > 0) {

    # calculation growing values
    growth_temp <- int_seagrass_growth(nutrients = uptake_total_g[id_bg_growth],
                                       gamma = 0.0082,
                                       slough_ratio = parameters$bg_slough_ratio)

    # increase biomass
    seafloor_values$bg_biomass[id_bg_growth] <-
      seafloor_values$bg_biomass[id_bg_growth] + growth_temp$biomass

    # increase detritus pool
    seafloor_values$detritus_pool[id_bg_growth] <-
      seafloor_values$detritus_pool[id_bg_growth] + growth_temp$detritus

    # remove nutrients used for growth from water column
    seafloor_values$wc_nutrients[id_bg_growth] <-
      seafloor_values$wc_nutrients[id_bg_growth] - uptake_total_g[id_bg_growth]

  }

  # above ground growth
  if (length(id_ag_growth) > 0) {

    # calculation growing values
    growth_temp <- int_seagrass_growth(nutrients = uptake_total_g[id_ag_growth],
                                       gamma = 0.0144,
                                       slough_ratio = parameters$ag_slough_ratio)

    # increase biomass
    seafloor_values$ag_biomass[id_ag_growth] <-
      seafloor_values$ag_biomass[id_ag_growth] + growth_temp$biomass

    # increase detritus pool
    seafloor_values$detritus_pool[id_ag_growth] <-
      seafloor_values$detritus_pool[id_ag_growth] + growth_temp$detritus

    # remove nutrients used for growth from water column
    seafloor_values$wc_nutrients[id_ag_growth] <-
      seafloor_values$wc_nutrients[id_ag_growth] - uptake_total_g[id_ag_growth]

  }

  # check if reef cells are available
  if (length(cells_reef) > 0) {

    # set reef values to old values
    seafloor_values$ag_biomass[cells_reef] <- reef_ag
    seafloor_values$bg_biomass[cells_reef] <- reef_bg
    seafloor_values$detritus_pool[cells_reef] <- reef_detritus
    seafloor_values$detritus_dead[cells_reef] <- reef_dead
    seafloor_values$wc_nutrients[cells_reef] <- reef_nutr

  }

  return(seafloor_values)
}
