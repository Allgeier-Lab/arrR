#' seagrass_accel
#'
#' @description Internal function to simulate accelerated seagrass
#'
#' @param biomass_dry Add info about parameter.
#' @param nutrients Add info about parameter.
#' @param max_biomass Add info about parameter.
#' @param sg_density Add info about parameter.
#' @param sigmoid_slope Add info about parameter.
#' @param v_max Add info about parameter.
#' @param nutrients_thres Add info about parameter.
#' @param k_max Add info about parameter.
#' @param gamma Add info about parameter.
#' @param slough_ratio Add info about parameter.
#' @param slough_detritus_ratio Add info about parameter.
#'
#' @details
#' Internal function to simulate above ground seagrass processes. This
#' includes i) ... ii) ...
#'
#' @return RasterBrick
#'
#' @aliases int_seagrass_accel
#' @rdname int_seagrass_accel
#'
#' @keywords internal
#'
#' @export
int_seagrass_accel <- function(biomass_dry_bg, biomass_dry_ag, biomass_max_bg,
                               nutrients, nutrients_thres,
                               sigmoid_slope, v_max, k_max, gamma,
                               slough_ratio, slough_detritus_ratio) {

  # calculate maximum biomass
  biomass_diff_bg <- biomass_dry_bg - biomass_max_bg # (450 + 0.039) / 0.0941

  # reclassify difference between current and maximum biomass
  # MH: Why is the threshold 20? And why do only two possible values exist?
  biomass_diff_bg <- ifelse(test = biomass_diff_bg >= 20, yes = 20, no = -20)

  # calculate update
  uptake <- (1 / (1 + exp((-1) * sigmoid_slope * biomass_diff_bg))) *
    (v_max * (nutrients - nutrients_thres) /
       (k_max + (nutrients - nutrients_thres))) * biomass_dry_ag

  # set blade uptake to -nutrients if it exceeds available nutrients
  uptake_exceed <- which(uptake > (nutrients * 10000))

  uptake[uptake_exceed] <- nutrients[uptake_exceed] * 10000 - 0.001

  # convert update to wet biomass
  biomass_wet <- int_convert_n(uptake, to = "g") * (gamma ^ -1)

  # calculate slough amount of blades
  blade_slough <- ifelse(biomass_wet > 0,
                         yes = biomass_wet * slough_ratio,
                         no = 0)

  # remove blade slough from growth biomass
  biomass_wet <- biomass_wet - blade_slough

  # calculate detritus amount
  detritus <- blade_slough * slough_detritus_ratio * gamma

  # remove nutrients from wc pool
  nutrients <- (-1) * int_convert_n(uptake, to = "g") +
    (1 - slough_detritus_ratio) * blade_slough * gamma

  return(list(biomass_wet = biomass_wet,
              detritus = detritus,
              nutrients = nutrients))
}
