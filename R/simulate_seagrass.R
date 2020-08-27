#' simulate_seagrass
#'
#' @description Simulate seagrass.
#'
#' @param seafloor Environment created with \code{\link{setup_environment}}.
#' @param parameters List with all model parameters.
#' @param min_per_i Integer to specify minutes per i.
#' @param verbose If TRUE, progress reports are printed.
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
simulate_seagrass <- function(seafloor, parameters, min_per_i, verbose = TRUE) {

  # # get all environmental values of non-reef cells
  # environment_dt <- int_as_data_table_ras(environment)

  # environment_dt <- environment_dt[reef == 0] # get rid of reef cells here?

  # convert water coloumn nutrients to umol/l
  # MH: Why is the value in int_convert_n 18.039?
  nutrients <- int_convert_n(parameters$wc_nutrients, to = "umol") / 10000

  nutrients_thres_ag <- int_convert_n(x = parameters$ag_nutrients_thres,
                                     to = "umol") / 10000

  nutrients_thres_b <- int_convert_n(x = parameters$bg_nutrients_thres_b,
                                     to = "umol") / 10000

  # convert wet to dry biomass
  biomass_dry_ag <- int_convert_dry(x = seafloor$ag_biomass[],
                                    what = "above")

  biomass_dry_bg <- int_convert_dry(x = seafloor$bg_biomass[],
                                    what = "below")

  # MH: Why is this not a parameter as ag?
  biomass_max_bg = (450 + 0.039) / 0.0941

  v_max_ag_a <- parameters$ag_v_max_a / 60 * min_per_i
  v_max_ag_b <- parameters$ag_v_max_b / 60 * min_per_i

  v_max_bg <- parameters$bg_v_max / 60 * min_per_i


  # simulate above ground seagrass
  # MH: This is still missing the accelerated growth
  seagrass_ag <- int_seagrass_ag(biomass_dry = biomass_dry_ag,
                                 max_biomass = parameters$ag_max,
                                 sg_density = parameters$sg_density,
                                 sigmoid_slope = parameters$ag_sigmoid_slope_a,
                                 v_max = v_max_ag_a,
                                 nutrients = nutrients,
                                 nutrients_thres = nutrients_thres_ag,
                                 k_max = parameters$ag_k_max_a,
                                 gamma = parameters$ag_gamma,
                                 slough_ratio = parameters$ag_slough_ratio,
                                 slough_detritus_ratio = parameters$slough_detritus_ratio,
                                 verbose = verbose)

  # simulate below ground seagrass
  # MH: This is still missing the accelerated growth
  seagrass_bg <- int_seagrass_bg(biomass_dry = biomass_dry_bg,
                                 sigmoid_slope = parameters$bg_sigmoid_slope,
                                 nutrients = nutrients,
                                 nutrients_thres_a = parameters$bg_nutrients_thres_a,
                                 nutrients_thres_b = nutrients_thres_b,
                                 v_max = v_max_bg,
                                 k_max = parameters$bg_k_max,
                                 gamma = parameters$bg_gamma,
                                 slough_ratio = parameters$bg_slough_ratio,
                                 slough_detritus_ratio = parameters$slough_detritus_ratio,
                                 verbose = verbose)

  # M: What is this?
  seagrass_accl <- int_seagrass_accel(biomass_dry_ag = biomass_dry_ag,
                                      biomass_dry_bg = biomass_dry_bg,
                                      biomass_max_bg = biomass_max_bg,
                                      nutrients = nutrients,
                                      nutrients_thres = nutrients_thres_ag,
                                      sigmoid_slope = parameters$ag_sigmoid_slope_b,
                                      v_max = v_max_ag_b,
                                      k_max = parameters$ag_k_max_b,
                                      gamma = parameters$ag_gamma,
                                      slough_ratio = parameters$ag_slough_ratio,
                                      slough_detritus_ratio = parameters$slough_detritus_ratio,
                                      verbose = verbose)

  # update environment RasterBrick
  seafloor$ag_biomass <- seafloor$ag_biomass + seagrass_ag$biomass_wet +
    seagrass_accl$biomass_wet

  seafloor$bg_biomass <- seafloor$bg_biomass + seagrass_bg$biomass_wet

  seafloor$detrital_pool <- seafloor$detrital_pool + seagrass_ag$detritus +
    seagrass_bg$detritus + seagrass_accl$detritus

  seafloor$wc_nutrients <- seafloor$wc_nutrients + seagrass_ag$nutrients +
    seagrass_bg$nutrients + seagrass_accl$nutrients


  # set environmental values of AR cells to 0
  seafloor[seafloor$reef == 1][, 1:4] <- 0

  return(seafloor)
}
