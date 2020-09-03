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

  # convert water coloumn nutrients to umol/l
  # MH: Why is the value in int_convert_n 18.039?
  wc_nutrients <- int_convert_n(parameters$wc_nutrients, to = "umol") / 10000

  ag_nutrients_thres <- int_convert_n(x = parameters$ag_nutrients_thres,
                                      to = "umol") / 10000

  bg_nutrients_thres_b <- int_convert_n(x = parameters$bg_nutrients_thres_b,
                                        to = "umol") / 10000

  # convert wet to dry biomass
  ag_biomass_dry <- int_convert_dry(x = seafloor_values[, 1],
                                    what = "above")

  bg_biomass_dry <- int_convert_dry(x = seafloor_values[, 2],
                                    what = "below")

  # MH: Why is this not a parameter as ag?
  bg_biomass_max = (450 + 0.039) / 0.0941

  ag_v_max_a <- parameters$ag_v_max_a / 60 * min_per_i
  ag_v_max_b <- parameters$ag_v_max_b / 60 * min_per_i

  bg_v_max <- parameters$bg_v_max / 60 * min_per_i

  # simulate above ground seagrass
  # MH: This is still missing the accelerated growth
  seagrass_ag <- int_seagrass_ag(biomass_dry = ag_biomass_dry,
                                 max_biomass = parameters$ag_biomass_max,
                                 sg_density = parameters$sg_density,
                                 sigmoid_slope = parameters$ag_sigmoid_slope_a,
                                 v_max = ag_v_max_a,
                                 nutrients = wc_nutrients,
                                 nutrients_thres = ag_nutrients_thres,
                                 k_max = parameters$ag_k_max_a,
                                 gamma = parameters$ag_gamma,
                                 slough_ratio = parameters$ag_slough_ratio,
                                 slough_detritus_ratio = parameters$slough_detritus_ratio)

  # simulate below ground seagrass
  seagrass_bg <- int_seagrass_bg(biomass_dry = bg_biomass_dry,
                                 sigmoid_slope = parameters$bg_sigmoid_slope,
                                 nutrients = wc_nutrients,
                                 nutrients_thres_a = parameters$bg_nutrients_thres_a,
                                 nutrients_thres_b = bg_nutrients_thres_b,
                                 v_max = bg_v_max,
                                 k_max = parameters$bg_k_max,
                                 gamma = parameters$bg_gamma,
                                 slough_ratio = parameters$bg_slough_ratio,
                                 slough_detritus_ratio = parameters$slough_detritus_ratio)

  # MH: What is this?
  seagrass_accl <- int_seagrass_accel(biomass_dry_ag = ag_biomass_dry,
                                      biomass_dry_bg = bg_biomass_dry,
                                      biomass_max_bg = bg_biomass_max,
                                      nutrients = wc_nutrients,
                                      nutrients_thres = ag_nutrients_thres,
                                      sigmoid_slope = parameters$ag_sigmoid_slope_b,
                                      v_max = ag_v_max_b,
                                      k_max = parameters$ag_k_max_b,
                                      gamma = parameters$ag_gamma,
                                      slough_ratio = parameters$ag_slough_ratio,
                                      slough_detritus_ratio = parameters$slough_detritus_ratio)

  # check if reef cells are available
  if (length(cells_reef) > 0) {

    # get current value of reef cells
    reef_values <- seafloor_values[cells_reef, -6]

  }

  # update seafloor values
  seafloor_values[, c(-4, -6)] <- seafloor_values[, c(-4, -6)] +
    cbind((seagrass_ag$biomass_wet + seagrass_accl$biomass_wet),
          seagrass_bg$biomass_wet,
          (seagrass_ag$detritus + seagrass_bg$detritus + seagrass_accl$detritus),
          (seagrass_ag$nutrients + seagrass_bg$nutrients + seagrass_accl$nutrients))

  # check if reef cells are available
  if (length(cells_reef) > 0) {

    # set reef values to old values
    seafloor_values[cells_reef, -6] <- reef_values

  }

  # update environment RasterBrick
  raster::values(seafloor) <- seafloor_values

  return(seafloor)
}
