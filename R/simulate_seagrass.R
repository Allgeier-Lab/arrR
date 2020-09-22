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
  seafloor_values <- raster::values(seafloor)

  # check if reef cells are available
  if (length(cells_reef) > 0) {

    # get current value of reef cells
    reef_values <- seafloor_values[cells_reef, -c(6, 7)]

  }

  # convert water column nutrients to umol/l
  wc_nutrients_umol <- int_convert_nutr(seafloor_values[, "wc_nutrients"],
                                        to = "umol") / 10000

  # convert uptake parameters to correct tick scale
  # MH: No need to do this within loop
  ag_v_max <- parameters$ag_v_max / 60 * min_per_i

  bg_v_max <- parameters$bg_v_max / 60 * min_per_i

  # calculate bg and ag uptake depending on nutrients and biomass
  uptake_bg <- (bg_v_max * wc_nutrients_umol /
                  (parameters$bg_k_max + wc_nutrients_umol)) *
    seafloor_values[, "bg_biomass"]

  uptake_ag <- (ag_v_max * wc_nutrients_umol /
                  (parameters$ag_k_max + wc_nutrients_umol)) *
    seafloor_values[, "ag_biomass"]

  # sum bg and ag to get total uptake
  uptake_total <- int_convert_nutr(x = uptake_bg + uptake_ag, to = "g")

  # get cell ids for different growing behaviors
  id_bg_growth <- which(seafloor_values[, "bg_biomass"] <= parameters$bg_biomass_max)

  id_ag_growth <- which(seafloor_values[, "bg_biomass"] > parameters$bg_biomass_max &
                          seafloor_values[, "ag_biomass"] <= parameters$ag_biomass_thres)

  id_bg_decrease <- which(seafloor_values[, "bg_biomass"] > parameters$bg_biomass_max &
                            seafloor_values[, "ag_biomass"] > parameters$ag_biomass_thres)

  # get difference between current and maximum biomass/threshold
  bg_biomass_diff <- 1 - ((parameters$bg_biomass_max -
                             seafloor_values[, "bg_biomass"]) /
                            parameters$bg_biomass_max)

  ag_biomass_diff_thres <- 1 - ((parameters$ag_biomass_thres -
                                   seafloor_values[, "ag_biomass"]) /
                                  parameters$ag_biomass_thres)

  ag_biomass_diff <- 1 - ((parameters$ag_biomass_max -
                             seafloor_values[, "ag_biomass"]) /
                            parameters$ag_biomass_max)

  # below ground growth

  # calculate ratio allocation bg
  bg_biomass_sigm <- int_calc_sigmoid(x = bg_biomass_diff[id_bg_growth],
                                      log_slope = parameters$bg_sigmoid_slope)

  # calculate ratio allocation ag
  ag_biomass_sigm <- 0 # bg_biomass_sigm * -1

  result_temp <- int_seagrass_growth(biomass_growth = seafloor_values[id_bg_growth, "bg_biomass"],
                                     biomass_reduction = seafloor_values[id_bg_growth, "ag_biomass"],
                                     nutrients = uptake_total[id_bg_growth],
                                     growth_fraction = bg_biomass_sigm,
                                     reduction_fraction = ag_biomass_sigm,
                                     gamma = 0.0082,
                                     slough_ratio = parameters$bg_slough_ratio,
                                     slough_detritus_ratio = parameters$slough_detritus_ratio,
                                     reduction = parameters$ag_reduction)

  # update values
  seafloor_values[id_bg_growth, c("ag_biomass", "bg_biomass",
                                  "detritus_pool", "wc_nutrients")] <-
    seafloor_values[id_bg_growth, c("ag_biomass", "bg_biomass",
                                    "detritus_pool", "wc_nutrients")] +
    cbind(result_temp$reduction, result_temp$growth,
          result_temp$detritus, result_temp$nutrients)

  # above ground growth

  # calculate ratio allocation ag
  ag_biomass_sigm <- int_calc_sigmoid(x = ag_biomass_diff_thres[id_ag_growth],
                                      log_slope = parameters$ag_sigmoid_slope)

  # calculate ratio allocation bg
  bg_biomass_sigm <- 0

  result_temp <- int_seagrass_growth(biomass_growth = seafloor_values[id_ag_growth, "ag_biomass"],
                                     biomass_reduction = seafloor_values[id_ag_growth, "bg_biomass"],
                                     nutrients = uptake_total[id_ag_growth],
                                     growth_fraction = ag_biomass_sigm,
                                     reduction_fraction = bg_biomass_sigm,
                                     gamma = 0.0144,
                                     slough_ratio = parameters$ag_slough_ratio,
                                     slough_detritus_ratio = parameters$slough_detritus_ratio,
                                     reduction = parameters$bg_reduction)

  # update values
  seafloor_values[id_ag_growth, c("ag_biomass", "bg_biomass",
                                  "detritus_pool", "wc_nutrients")] <-
    seafloor_values[id_ag_growth, c("ag_biomass", "bg_biomass",
                                    "detritus_pool", "wc_nutrients")] +
    cbind(result_temp$growth, result_temp$reduction,
          result_temp$detritus, result_temp$nutrients)

  # above ground growth; below ground reduction

  # calculate ratio allocation ag
  ag_biomass_sigm <- int_calc_sigmoid(x = ag_biomass_diff[id_bg_decrease],
                                      log_slope = parameters$ag_sigmoid_slope)

  # calculate ratio allocation bg
  bg_biomass_sigm <- 0 # ag_biomass_sigm * -1

  result_temp <- int_seagrass_growth(biomass_growth = seafloor_values[id_bg_decrease, "ag_biomass"],
                                     biomass_reduction = seafloor_values[id_bg_decrease, "bg_biomass"],
                                     nutrients = uptake_total[id_bg_decrease],
                                     growth_fraction = ag_biomass_sigm,
                                     reduction_fraction = bg_biomass_sigm,
                                     gamma = 0.0144,
                                     slough_ratio = parameters$ag_slough_ratio,
                                     slough_detritus_ratio = parameters$slough_detritus_ratio,
                                     reduction = parameters$bg_reduction)

  # update values
  seafloor_values[id_bg_decrease, c("ag_biomass", "bg_biomass",
                                    "detritus_pool", "wc_nutrients")] <-
    seafloor_values[id_bg_decrease, c("ag_biomass", "bg_biomass",
                                      "detritus_pool", "wc_nutrients")] +
    cbind(result_temp$growth, result_temp$reduction,
          result_temp$detritus, result_temp$nutrients)

  # check if reef cells are available
  if (length(cells_reef) > 0) {

    # set reef values to old values
    seafloor_values[cells_reef, -c(6, 7)] <- reef_values

  }

  # update environment RasterBrick
  raster::values(seafloor) <- seafloor_values

  return(seafloor)
}
