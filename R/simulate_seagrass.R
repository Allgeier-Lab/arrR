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

  # calculate total possible nutrient uptake bg
  bg_uptake <- calc_nutr_uptake(nutrients = seafloor_values$nutrients_pool,
                                biomass = seafloor_values$bg_biomass,
                                v_max = parameters$bg_v_max,
                                k_m = parameters$bg_k_m,
                                time_fac = min_per_i / 60)

  # remove bg nutrient uptake
  seafloor_values$nutrients_pool <- seafloor_values$nutrients_pool - bg_uptake

  # calculate total possible nutrient uptake ag
  ag_uptake <- calc_nutr_uptake(nutrients = seafloor_values$nutrients_pool,
                                biomass = seafloor_values$ag_biomass,
                                v_max = parameters$ag_v_max,
                                k_m = parameters$ag_k_m,
                                time_fac = min_per_i / 60)

  # remove ag nutrient uptake
  seafloor_values$nutrients_pool <- seafloor_values$nutrients_pool - ag_uptake

  # sum total uptake
  total_uptake_g <- bg_uptake + ag_uptake

  # # check if total uptake exceeds total available nutrients
  # total_uptake_g <- ifelse(test = total_uptake_g > seafloor_values$nutrients,
  #                          yes = seafloor_values$nutrients, no = total_uptake_g)
  #
  # # remove nutrients used for ag and bg growth from water column
  # seafloor_values$nutrients_pool <- seafloor_values$nutrients_pool - total_uptake_g

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

  # calculate nutrients of total detritus
  total_detritus <- (bg_detritus * 0.0082) + (ag_detritus * 0.0144)

  # remove detritus from bg biomass
  seafloor_values$bg_biomass <- seafloor_values$bg_biomass - bg_detritus

  # remove detritus from ag biomass
  seafloor_values$ag_biomass <- seafloor_values$ag_biomass - ag_detritus

  # save slough amount nutrients
  seafloor_values$slough <- seafloor_values$slough + total_detritus

  # add nutrients to detritus pool
  seafloor_values$detritus_pool <- seafloor_values$detritus_pool + total_detritus

  # check in which cells nutrients used for bg growth only
  # i) bg biomass below threshold, but uptake not enough to keep bg/ag stable
  # ii) bg biomass above threshold, but uptake not enough to keep bg stable
  id_bg_growth_only <- c(which(bg_modf > (1 - parameters$bg_thres) &
                                 total_uptake_g <= total_detritus),
                         which(bg_modf <= (1 - parameters$bg_thres) &
                                 total_uptake_g <= (bg_detritus * 0.0082)))

  # check in which cells nutrients used for bg growth and stable ag biomass
  # iii) bg biomass below threshold and uptake large enough to keep bg/ag stable
  id_bg_growth <- which(bg_modf > (1 - parameters$bg_thres) &
                          total_uptake_g > total_detritus)

  # check in which cells nutrients are shared between ag and bg
  # iv) bg biomass above threshold and uptake large enough to keep bg stable
  id_shared_growth <- which(bg_modf <= (1 - parameters$bg_thres) &
                              total_uptake_g > (bg_detritus * 0.0082))

  # only nutrients for bg growth
  if (length(id_bg_growth_only) > 0) {

    seafloor_values$bg_biomass[id_bg_growth_only] <-
      seafloor_values$bg_biomass[id_bg_growth_only] + (total_uptake_g[id_bg_growth_only] / 0.0082)

  }

  # stable ag biomass and biomass growth
  if (length(id_bg_growth) > 0) {

    # add detritus fraction for stable ag biomass
    seafloor_values$ag_biomass[id_bg_growth] <-
      seafloor_values$ag_biomass[id_bg_growth] + ag_detritus[id_bg_growth]

    # calculate remaining nutrients
    uptake_temp <- total_uptake_g[id_bg_growth] - (ag_detritus[id_bg_growth] * 0.0144)

    # growth of bg biomass with remaining uptake
    seafloor_values$bg_biomass[id_bg_growth] <-
      seafloor_values$bg_biomass[id_bg_growth] + (uptake_temp / 0.0082)

  }

  if (length(id_shared_growth) > 0) {

    # add detritus fraction for stable bg biomass
    seafloor_values$bg_biomass[id_shared_growth] <-
      seafloor_values$bg_biomass[id_shared_growth] + bg_detritus[id_shared_growth]

    # calculate remaining nutrients
    uptake_temp <- total_uptake_g[id_shared_growth] -
      (bg_detritus[id_shared_growth] * 0.0082)

    # calculate bg growth with remaining nutrients
    seafloor_values$bg_biomass[id_shared_growth] <-
      seafloor_values$bg_biomass[id_shared_growth] +
      ((uptake_temp * bg_modf[id_shared_growth]) / 0.0082)

    # calculate ag growth with remaining nutrients
    seafloor_values$ag_biomass[id_shared_growth] <-
      seafloor_values$ag_biomass[id_shared_growth] +
      ((uptake_temp * (1 - bg_modf[id_shared_growth])) / 0.0144)

  }

  # check if bg/ag biomass is above max
  seafloor_values <- check_max_biomass(seafloor_values = seafloor_values,
                                       biomass_max = c(parameters$bg_biomass_max,
                                                       parameters$ag_biomass_max),
                                       gamma = c(0.0082, 0.0144))

  # check if reef cells are available
  if (length(cells_reef) > 0) {

    # set reef values to old values
    seafloor_values$nutrients_pool[cells_reef] <- reef_nutr
    seafloor_values$detritus_pool[cells_reef] <- reef_detritus
    seafloor_values$detritus_dead[cells_reef] <- reef_dead

  }

  return(seafloor_values)
}
