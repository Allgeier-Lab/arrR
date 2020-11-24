#' simulate_seagrass
#'
#' @description Simulate seagrass.
#'
#' @param seafloor_values Matrix with seafloor values.
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
#' @return Matrix
#'
#' @aliases simulate_seagrass
#' @rdname simulate_seagrass
#'
#' @export
simulate_seagrass <- function(seafloor_values, parameters, cells_reef, min_per_i) {

  # check if reef cells are available
  if (length(cells_reef) > 0) {

    # get current value of reef cells
    reef_values <- seafloor_values[cells_reef, c("nutrients_pool",
                                                 "detritus_pool",
                                                 "detritus_dead")]

  }

  # get all needed seafloor values
  bg_biomass_temp <- seafloor_values[, "bg_biomass"]

  ag_biomass_temp <- seafloor_values[, "ag_biomass"]

  nutrients_pool_temp <- seafloor_values[, "nutrients_pool"]

  slough_temp <- seafloor_values[, "slough"]

  detritus_pool_temp <- seafloor_values[, "detritus_pool"]

  # calculate total possible nutrient uptake bg
  bg_uptake <- calc_nutr_uptake(nutrients = nutrients_pool_temp,
                                biomass = bg_biomass_temp,
                                v_max = parameters$bg_v_max,
                                k_m = parameters$bg_k_m,
                                time_fac = min_per_i / 60)

  # calculate total possible nutrient uptake ag
  ag_uptake <- calc_nutr_uptake(nutrients = (nutrients_pool_temp - bg_uptake),
                                biomass = ag_biomass_temp,
                                v_max = parameters$ag_v_max,
                                k_m = parameters$ag_k_m,
                                time_fac = min_per_i / 60)

  # sum total uptake
  total_uptake_g <- bg_uptake + ag_uptake

  # remove uptake
  nutrients_pool_temp <- nutrients_pool_temp - total_uptake_g

  # # check if total uptake exceeds total available nutrients
  # total_uptake_g <- ifelse(test = total_uptake_g > seafloor_values$nutrients,
  #                          yes = seafloor_values$nutrients, no = total_uptake_g)
  #
  # # remove nutrients used for ag and bg growth from water column
  # seafloor_values$nutrients_pool <- seafloor_values$nutrients_pool - total_uptake_g

  # calculate bg detritus modifier
  bg_modf <- (parameters$bg_biomass_max - bg_biomass_temp) /
    (parameters$bg_biomass_max - parameters$bg_biomass_min)

  # calculate ag detritus modifier
  ag_modf <- (parameters$ag_biomass_max - ag_biomass_temp) /
    (parameters$ag_biomass_max - parameters$ag_biomass_min)

  # calculate detritus fraction from bg biomass
  bg_detritus <- bg_biomass_temp * (parameters$detritus_ratio * (1 - bg_modf))

  # calculate detritus fraction from ag biomass
  ag_detritus <- ag_biomass_temp * (parameters$detritus_ratio * (1 - ag_modf))

  # calculate nutrients of total detritus
  total_detritus <- (bg_detritus * 0.0082) + (ag_detritus * 0.0144)

  # remove detritus from bg biomass
  bg_biomass_temp <- bg_biomass_temp - bg_detritus

  # remove detritus from ag biomass
  ag_biomass_temp <- ag_biomass_temp - ag_detritus

  # save slough amount nutrients
  slough_temp <- slough_temp + total_detritus

  # add nutrients to detritus pool
  detritus_pool_temp <- detritus_pool_temp + total_detritus

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

    bg_biomass_temp[id_bg_growth_only] <- bg_biomass_temp[id_bg_growth_only] +
      (total_uptake_g[id_bg_growth_only] / 0.0082)

  }

  # stable ag biomass and biomass growth
  if (length(id_bg_growth) > 0) {

    # add detritus fraction for stable ag biomass
    ag_biomass_temp[id_bg_growth] <- ag_biomass_temp[id_bg_growth] + ag_detritus[id_bg_growth]

    # calculate remaining nutrients
    uptake_temp <- total_uptake_g[id_bg_growth] - (ag_detritus[id_bg_growth] * 0.0144)

    # growth of bg biomass with remaining uptake
    bg_biomass_temp[id_bg_growth] <- bg_biomass_temp[id_bg_growth] + (uptake_temp / 0.0082)

  }

  if (length(id_shared_growth) > 0) {

    # add detritus fraction for stable bg biomass
    bg_biomass_temp[id_shared_growth] <- bg_biomass_temp[id_shared_growth] +
      bg_detritus[id_shared_growth]

    # calculate remaining nutrients
    uptake_temp <- total_uptake_g[id_shared_growth] -
      (bg_detritus[id_shared_growth] * 0.0082)

    # calculate bg growth with remaining nutrients
    bg_biomass_temp[id_shared_growth] <- bg_biomass_temp[id_shared_growth] +
      ((uptake_temp * bg_modf[id_shared_growth]) / 0.0082)

    # calculate ag growth with remaining nutrients
    ag_biomass_temp[id_shared_growth] <- ag_biomass_temp[id_shared_growth] +
      ((uptake_temp * (1 - bg_modf[id_shared_growth])) / 0.0144)

  }

  # check if bg/ag biomass is above max
  seafloor_values <- check_max_biomass(seafloor_values = seafloor_values,
                                       biomass_max = c(parameters$bg_biomass_max,
                                                       parameters$ag_biomass_max),
                                       gamma = c(0.0082, 0.0144))


  # update seafloor values
  seafloor_values[, "bg_biomass"] <- bg_biomass_temp

  seafloor_values[, "ag_biomass"] <- ag_biomass_temp

  seafloor_values[, "nutrients_pool"] <- nutrients_pool_temp

  seafloor_values[, "slough"] <- slough_temp

  seafloor_values[, "detritus_pool"] <- detritus_pool_temp

  # check if reef cells are available
  if (length(cells_reef) > 0) {

    # set reef values to old values
    seafloor_values[cells_reef, c("nutrients_pool",
                                  "detritus_pool",
                                  "detritus_dead")] <- reef_values

  }

  return(seafloor_values)
}
