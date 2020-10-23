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
    reef_nutr <- seafloor_values$nutrients_pool[cells_reef]
    reef_detritus <- seafloor_values$detritus_pool[cells_reef]
    reef_dead <- seafloor_values$detritus_dead[cells_reef]

  }

  # MH: Check what happens if bg nutrients are removed from pool before ag is calc

  # calculate total possible nutrient uptake bg
  bg_uptake <- calc_nutr_uptake(nutrients = seafloor_values$nutrients_pool,
                                biomass = seafloor_values$bg_biomass,
                                v_max = parameters$bg_v_max,
                                k_m = parameters$bg_k_m,
                                time_fac = min_per_i / 60)

  # calculate total possible nutrient uptake ag
  ag_uptake <- calc_nutr_uptake(nutrients = seafloor_values$nutrients_pool,
                                biomass = seafloor_values$ag_biomass,
                                v_max = parameters$ag_v_max,
                                k_m = parameters$ag_k_m,
                                time_fac = min_per_i / 60)

  # sum total uptake
  total_uptake_g <- bg_uptake + ag_uptake

  # check if total uptake exceeds total available nutrients
  total_uptake_g <- ifelse(test = total_uptake_g > seafloor_values$nutrients,
                           yes = seafloor_values$nutrients, no = total_uptake_g)

  # remove nutrients used for ag and bg growth from water column
  seafloor_values$nutrients_pool <- seafloor_values$nutrients_pool - total_uptake_g

  # calculate ratio with giving priority to bg; inverse is used for ag
  bg_share <- (parameters$bg_biomass_max - seafloor_values$bg_biomass) /
    (parameters$bg_biomass_max - parameters$bg_biomass_min)

  # calculate bg growth in biomass; divide nutrients uptake by factor of N% per biomass
  bg_growth <- (total_uptake_g * bg_share) / 0.0082

  # increase bg biomass
  seafloor_values$bg_biomass <- seafloor_values$bg_biomass + bg_growth

  # calculate ag growth in biomass; divide nutrients uptake by factor of N% per biomass
  ag_growth <- (total_uptake_g * (1 - bg_share)) / 0.0144

  # increase ag biomass
  seafloor_values$ag_biomass <- seafloor_values$ag_biomass + ag_growth

  # check if biomass is above max
  ag_max_id <- which(seafloor_values$ag_biomass > parameters$ag_biomass_max)

  # set to ag biomass to max biomass and add biomass to bg first
  if (length(ag_max_id) > 0) {

    # get difference between current and max
    ag_diff <- seafloor_values$ag_biomass[ag_max_id] - parameters$ag_biomass_max

    # remove difference ag biomass
    seafloor_values$ag_biomass[ag_max_id] <-
      seafloor_values$ag_biomass[ag_max_id] - ag_diff

    # add difference to bg biomass
    seafloor_values$bg_biomass[ag_max_id] <-
      seafloor_values$bg_biomass[ag_max_id] + ag_diff

  }

  # check if biomass is above max
  bg_max_id <- which(seafloor_values$bg_biomass > parameters$bg_biomass_max)

  # set to bg biomass to max biomass and add directly to detritus
  if (length(bg_max_id) > 0) {

    # calculate difference between current and max
    bg_diff <- seafloor_values$bg_biomass[bg_max_id] - parameters$bg_biomass_max

    # remove difference from biomass
    seafloor_values$bg_biomass[bg_max_id] <-
      seafloor_values$bg_biomass[bg_max_id] - bg_diff

    # add nutrients of biomass to detritus pool
    seafloor_values$detritus_pool[bg_max_id] <-
      seafloor_values$detritus_pool[bg_max_id] + (bg_diff * 0.0082)

  }

  # calculate detritus modifier depending on current and and maximum biomass
  bg_detr_modf <- (seafloor_values$bg_biomass - parameters$bg_biomass_min) /
                    (parameters$bg_biomass_max - parameters$bg_biomass_min)

  ag_detr_modf <- (seafloor_values$ag_biomass - parameters$ag_biomass_min) /
                    (parameters$ag_biomass_max - parameters$ag_biomass_min)

  # calculate biomass of detritus ratio depending on difference to maximum biomass
  bg_detritus <- seafloor_values$bg_biomass *
    (parameters$detritus_ratio * bg_detr_modf)

  ag_detritus <- seafloor_values$ag_biomass *
    (parameters$detritus_ratio * ag_detr_modf)

  # remove detritus from biomass
  seafloor_values$bg_biomass <- seafloor_values$bg_biomass - bg_detritus

  seafloor_values$ag_biomass <- seafloor_values$ag_biomass - ag_detritus

  # add nutrients to detritus pool
  seafloor_values$detritus_pool <- seafloor_values$detritus_pool +
    ((bg_detritus * 0.0082) + (ag_detritus * 0.0144))

  # check if reef cells are available
  if (length(cells_reef) > 0) {

    # set reef values to old values
    seafloor_values$nutrients_pool[cells_reef] <- reef_nutr
    seafloor_values$detritus_pool[cells_reef] <- reef_detritus
    seafloor_values$detritus_dead[cells_reef] <- reef_dead

  }

  return(seafloor_values)
}
