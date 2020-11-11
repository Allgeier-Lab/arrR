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
#' Function to simulate processes of aboveground and belowground seagrass growth and
#' slough.
#'
#' @references
#' DeAngelis, D.L., 1992. Dynamics of Nutrient Cycling and Food Webs. Springer
#' Netherlands, Dordrecht. https://doi.org/10.1007/978-94-011-2342-6
#'
#' @return data.frame
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

  # calculate bg detritus modifier
  bg_modf <- (parameters$bg_biomass_max - seafloor_values$bg_biomass) /
    (parameters$bg_biomass_max - parameters$bg_biomass_min)

  # calculate ag detritus modifier
  ag_modf <- (parameters$ag_biomass_max - seafloor_values$ag_biomass) /
    (parameters$ag_biomass_max - parameters$ag_biomass_min)

  # calculate detritus fraction from bg biomass
  bg_detritus <- seafloor_values$bg_biomass * (parameters$detritus_ratio * (1 - bg_modf))

  # calculate detritus fraction from ag biomass
  ag_detritus <- seafloor_values$ag_biomass * (parameters$detritus_ratio * (1 - ag_modf))

  # get cells for bg growth and stable ag biomass
  bg_growth_id_a <- which(seafloor_values$bg_biomass < parameters$bg_biomass_max &
                            total_uptake_g >= (ag_detritus * 0.0144))

  # get cells for bg growth only
  bg_growth_id_b <- which(seafloor_values$bg_biomass < parameters$bg_biomass_max &
                            total_uptake_g < (ag_detritus * 0.0144))

  # get cells for ag growth and stable bg biomass
  ag_growth_id_a <- which(seafloor_values$bg_biomass >= parameters$bg_biomass_max &
                            total_uptake_g >= (bg_detritus * 0.0082))

  # get cells for ag growth only
  ag_growth_id_b <- which(seafloor_values$bg_biomass >= parameters$bg_biomass_max &
                            total_uptake_g < (bg_detritus * 0.0082))

  # remove detritus from bg biomass
  seafloor_values$bg_biomass <- seafloor_values$bg_biomass - bg_detritus

  # remove detritus from ag biomass
  seafloor_values$ag_biomass <- seafloor_values$ag_biomass - ag_detritus

  # save slough amount nutrients
  seafloor_values$slough <- seafloor_values$slough +
    ((bg_detritus * 0.0082) + (ag_detritus * 0.0144))

  # add nutrients to detritus pool
  seafloor_values$detritus_pool <- seafloor_values$detritus_pool +
    ((bg_detritus * 0.0082) + (ag_detritus * 0.0144))

  # bg growth and stable ag biomass
  if (length(bg_growth_id_a) > 0) {

    # add detritus fraction for stable ag biomass
    seafloor_values$ag_biomass[bg_growth_id_a] <- seafloor_values$ag_biomass[bg_growth_id_a] +
      ag_detritus[bg_growth_id_a]

    # calculate bg growth with remaining nutrients
    seafloor_values$bg_biomass[bg_growth_id_a] <- seafloor_values$bg_biomass[bg_growth_id_a] +
      ((total_uptake_g[bg_growth_id_a] - (ag_detritus[bg_growth_id_a] * 0.0144)) / 0.0082)

  }

  # bg growth only
  if (length(bg_growth_id_b) > 0) {

    # calculate bg growth for cells with insufficient uptake to equalize ag detritus
    seafloor_values$bg_biomass[bg_growth_id_b] <- seafloor_values$bg_biomass[bg_growth_id_b] +
      (total_uptake_g[bg_growth_id_b] / 0.0082)

  }

  # ag growth and stable bg biomass
  if (length(ag_growth_id_a) > 0) {

    # add detritus fraction for stable bg biomass
    seafloor_values$bg_biomass[ag_growth_id_a] <- seafloor_values$bg_biomass[ag_growth_id_a] +
      bg_detritus[ag_growth_id_a]

    # calculate bg growth with remaining nutrients
    seafloor_values$ag_biomass[ag_growth_id_a] <- seafloor_values$ag_biomass[ag_growth_id_a] +
      ((total_uptake_g[ag_growth_id_a] - (bg_detritus[ag_growth_id_a] * 0.0082)) / 0.0144)

  }

  # ag growth only
  if (length(ag_growth_id_b) > 0) {

    # calculate bg growth with remaining nutrients
    seafloor_values$ag_biomass[ag_growth_id_b] <- seafloor_values$ag_biomass[ag_growth_id_b] +
      (total_uptake_g[ag_growth_id_b] / 0.0144)

  }

  # check if bg biomass is above max
  bg_max_id <- which(seafloor_values$bg_biomass > parameters$bg_biomass_max)

  # set to bg biomass to max biomass and add directly to detritus
  if (length(bg_max_id) > 0) {

    # calculate difference between current and max
    bg_diff <- seafloor_values$bg_biomass[bg_max_id] - parameters$bg_biomass_max

    # remove difference from biomass
    seafloor_values$bg_biomass[bg_max_id] <- seafloor_values$bg_biomass[bg_max_id] - bg_diff

    # add nutrients of biomass to detritus pool
    seafloor_values$detritus_pool[bg_max_id] <- seafloor_values$detritus_pool[bg_max_id] + (bg_diff * 0.0082)

  }

  # check if ag biomass is above max
  ag_max_id <- which(seafloor_values$ag_biomass > parameters$ag_biomass_max)

  # set to ag biomass to max biomass and add directly to detritus
  if (length(ag_max_id) > 0) {

    # calculate difference between current and max
    ag_diff <- seafloor_values$ag_biomass[ag_max_id] - parameters$ag_biomass_max

    # remove difference from biomass
    seafloor_values$ag_biomass[ag_max_id] <- seafloor_values$ag_biomass[ag_max_id] - ag_diff

    # add nutrients of biomass to detritus pool
    seafloor_values$detritus_pool[ag_max_id] <- seafloor_values$detritus_pool[ag_max_id] + (ag_diff * 0.0082)

  }

  # check if reef cells are available
  if (length(cells_reef) > 0) {

    # set reef values to old values
    seafloor_values$nutrients_pool[cells_reef] <- reef_nutr
    seafloor_values$detritus_pool[cells_reef] <- reef_detritus
    seafloor_values$detritus_dead[cells_reef] <- reef_dead

  }

  return(seafloor_values)
}
