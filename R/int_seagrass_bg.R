#' seagrass_bg
#'
#' @description Internal function to simulate above ground seagrass
#'
#' @param biomass_dry Add info about parameter.
#' @param nutrients Add info about parameter.
#' @param sigmoid_slope Add info about parameter.
#' @param nutrients_thres_a Add info about parameter.
#' @param nutrients_thres_b Add info about parameter.
#' @param v_max Add info about parameter.
#' @param k_max Add info about parameter.
#' @param gamma Add info about parameter.
#' @param slough_ratio Add info about parameter.
#' @param slough_detritus_ratio Add info about parameter.
#'
#' @details
#' Internal function to simulate below ground seagrass processes. This
#' includes i) ... ii) ...
#'
#' @return RasterBrick
#'
#' @aliases int_seagrass_bg
#' @rdname int_seagrass_bg
#'
#' @keywords internal
#'
#' @export
int_seagrass_bg <- function(biomass_dry, nutrients, sigmoid_slope,
                            nutrients_thres_a, nutrients_thres_b,
                            v_max, k_max, gamma, slough_ratio,
                            slough_detritus_ratio) {

  # calculate uptake
  # MH: Why are there two different thresholds?
  uptake <- (1 / (1 + exp( -1 * sigmoid_slope * (nutrients - nutrients_thres_a)))) *
    (v_max * (nutrients - nutrients_thres_b) / (k_max + (nutrients_thres_b))) * biomass_dry

  # set blade uptake to -nutrients if it exceeds available nutrients
  # MH: But this is not really what is happening here? Seems to be an issue with units?
  # uptake[uptake > nutrients * 10000] <- nutrients * 10000 - 0.001
  uptake_exceed <- which(uptake > (nutrients * 10000))

  uptake[uptake_exceed] <- nutrients[uptake_exceed] * 10000 - 0.001

  # convert update to wet biomass
  biomass_wet <- int_convert_n(uptake, to = "g") * (gamma ^ -1)

  # calculate slough amount of blades
  # MH: In NetLogo, this part is very error prone
  blade_slough <- ifelse(biomass_wet > 0,
                         yes = biomass_wet * slough_ratio,
                         no = 0)

  # remove blade slough from growth biomass
  biomass_wet <- biomass_wet - blade_slough

  # add remaining nutrients to pool
  # MH: This is negative?
  nutrients <- (-1) * int_convert_n(uptake, to = "g") +
    (1 - slough_detritus_ratio) * blade_slough * gamma

  # calculate detritus amount
  # MH: Whats gamma again?
  # MH: In NetLogo, this is very error prone
  detritus <- blade_slough * slough_detritus_ratio * gamma

  return(list(biomass_wet = biomass_wet,
              detritus = detritus,
              nutrients = nutrients))
}
