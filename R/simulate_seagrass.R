#' simulate_seagrass
#'
#' @description Simulate seagrass.
#'
#' @param seafloor Environment created with \code{\link{setup_seafloor}}.
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
simulate_seagrass <- function(seafloor, parameters, cells_reef, min_per_i) {

  # get seafloor values
  seafloor_values <- raster::values(seafloor)[, -c(4, 6, 7)]

  # check if reef cells are available
  if (length(cells_reef) > 0) {

    # get current value of reef cells
    reef_values <- seafloor_values[cells_reef, ]

  }

  # convert water column nutrients to umol/l
  wc_nutrients_umol <- int_convert_nutr(seafloor_values[, "wc_nutrients"],
                                        to = "umol") / 10000

  # convert uptake parameters to correct tick scale (from per h to day)
  # MH: No need to do this within loop
  ag_v_max <- parameters$ag_v_max / 60 * min_per_i # * 24

  bg_v_max <- parameters$bg_v_max / 60 * min_per_i # * 24

  # calculate bg and ag uptake depending on nutrients and biomass
  uptake_bg <- ((bg_v_max * wc_nutrients_umol) /
                  (parameters$bg_k_max + wc_nutrients_umol)) *
    seafloor_values[, "bg_biomass"]

  uptake_ag <- ((ag_v_max * wc_nutrients_umol) /
                  (parameters$ag_k_max + wc_nutrients_umol)) *
    seafloor_values[, "ag_biomass"]

  # sum bg and ag to get total uptake
  uptake_total <- int_convert_nutr(x = uptake_bg + uptake_ag, to = "g")

  # check if total uptake exceeds total available nutrients
  uptake_total <- ifelse(test = uptake_total > seafloor_values[, "wc_nutrients"],
                         yes = seafloor_values[, "wc_nutrients"], no = uptake_total)

  # get cell ids for different growing behaviors
  id_bg_growth <- which(seafloor_values[, "bg_biomass"] < parameters$bg_biomass_max)

  id_ag_growth <- which(seafloor_values[, "bg_biomass"] >= parameters$bg_biomass_max &
                          seafloor_values[, "ag_biomass"] < parameters$ag_biomass_thres)

  id_bg_decrease <- which(seafloor_values[, "bg_biomass"] >= parameters$bg_biomass_max &
                            seafloor_values[, "ag_biomass"] >= parameters$ag_biomass_thres &
                            seafloor_values[, "ag_biomass"] < parameters$ag_biomass_max)

  # below ground growth
  if (length(id_bg_growth) > 0) {

    # get difference between current and maximum biomass/threshold
    biomass_diff <- 1 - ((parameters$bg_biomass_max -
                            seafloor_values[id_bg_growth, "bg_biomass"]) /
                           parameters$bg_biomass_max)

    # calculate ratio allocation bg
    biomass_sigm_grow <- int_calc_sigmoid(x = biomass_diff,
                                          log_slope = parameters$bg_sigmoid_slope)

    # calculate ratio allocation ag
    biomass_sigm_redu <- biomass_sigm_grow * -1

    # calculation growing values
    result_temp <- int_seagrass_growth(biomass_growth = seafloor_values[id_bg_growth, "bg_biomass"],
                                       biomass_reduction = seafloor_values[id_bg_growth, "ag_biomass"],
                                       nutrients = uptake_total[id_bg_growth],
                                       growth_fraction = biomass_sigm_grow,
                                       reduction_fraction = biomass_sigm_redu,
                                       gamma = 0.0082,
                                       slough_ratio = parameters$bg_slough_ratio,
                                       slough_detritus_ratio = parameters$slough_detritus_ratio,
                                       reduction = parameters$ag_reduction)

    # update values
    seafloor_values[id_bg_growth, ] <- seafloor_values[id_bg_growth, ] +
      cbind(result_temp$reduction, result_temp$growth,
            result_temp$detritus, result_temp$nutrients)

  }

  # above ground growth
  if (length(id_ag_growth) > 0) {

    # get difference between current and maximum biomass/threshold
    biomass_diff <- 1 - ((parameters$ag_biomass_thres -
                            seafloor_values[id_ag_growth, "ag_biomass"]) /
                           parameters$ag_biomass_thres)

    # calculate ratio allocation ag
    biomass_sigm_grow <- int_calc_sigmoid(x = biomass_diff,
                                          log_slope = parameters$ag_sigmoid_slope)

    # calculate ratio allocation bg
    biomass_sigm_redu <- 0

    # calculation growing values
    result_temp <- int_seagrass_growth(biomass_growth = seafloor_values[id_ag_growth, "ag_biomass"],
                                       biomass_reduction = seafloor_values[id_ag_growth, "bg_biomass"],
                                       nutrients = uptake_total[id_ag_growth],
                                       growth_fraction = biomass_sigm_grow,
                                       reduction_fraction = biomass_sigm_redu,
                                       gamma = 0.0144,
                                       slough_ratio = parameters$ag_slough_ratio,
                                       slough_detritus_ratio = parameters$slough_detritus_ratio,
                                       reduction = parameters$bg_reduction)

    # update values
    seafloor_values[id_bg_growth, ] <- seafloor_values[id_bg_growth, ] +
      cbind(result_temp$growth, result_temp$reduction,
            result_temp$detritus, result_temp$nutrients)
  }

  # above ground growth; below ground reduction
  if (length(id_ag_growth) > 0) {

    # get difference between current and maximum biomass/threshold
    biomass_diff <- 1 - ((parameters$ag_biomass_max -
                            seafloor_values[id_bg_decrease, "ag_biomass"]) /
                           parameters$ag_biomass_max)

    # calculate ratio allocation ag
    biomass_sigm_grow <- int_calc_sigmoid(x = biomass_diff,
                                          log_slope = parameters$ag_sigmoid_slope)

    # calculate ratio allocation bg
    biomass_sigm_redu <- biomass_sigm_grow * -1

    # calculation growing values
    result_temp <- int_seagrass_growth(biomass_growth = seafloor_values[id_bg_decrease, "ag_biomass"],
                                       biomass_reduction = seafloor_values[id_bg_decrease, "bg_biomass"],
                                       nutrients = uptake_total[id_bg_decrease],
                                       growth_fraction = biomass_sigm_grow,
                                       reduction_fraction = biomass_sigm_redu,
                                       gamma = 0.0144,
                                       slough_ratio = parameters$ag_slough_ratio,
                                       slough_detritus_ratio = parameters$slough_detritus_ratio,
                                       reduction = parameters$bg_reduction)

    # update values
    seafloor_values[id_bg_growth, ] <- seafloor_values[id_bg_growth, ] +
      cbind(result_temp$growth, result_temp$reduction,
            result_temp$detritus, result_temp$nutrients)
  }

  # check if reef cells are available
  if (length(cells_reef) > 0) {

    # set reef values to old values
    seafloor_values[cells_reef, ] <- reef_values

  }

  # update environment RasterBrick
  raster::values(seafloor)[, -c(4, 6, 7)] <- seafloor_values

  return(seafloor)
}
