#' seagrass_ag
#'
#' @description Internal function to simulate above ground seagrass
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
#' @aliases int_seagrass_ag
#' @rdname int_seagrass_ag
#'
#' @keywords internal
#'
#' @export
int_seagrass_ag <- function(biomass_dry, nutrients, max_biomass, sg_density,
                            sigmoid_slope, v_max, nutrients_thres,
                            k_max, gamma, slough_ratio, slough_detritus_ratio) {

  # calculate difference between current and maximum biomass
  biomass_diff <- biomass_dry - max_biomass * sg_density

  # reclassify difference between current and maximum biomass
  # MH: Why is the threshold 20? And why do only two possible values exist?
  biomass_diff <- ifelse(test = biomass_diff >= 20, yes = 20, no = -20)

  # calculate uptake of nutrients by blades
  # MH: Where does the numbers for int_convert_dry come from?
  uptake <- (1 / (1 + exp(sigmoid_slope * biomass_diff))) *
    (v_max * (nutrients - nutrients_thres) /
       (k_max + (nutrients - nutrients_thres))) * biomass_dry

  # set blade uptake to XXX if it exceeds available nutrients
  # MH: But this is not really what is happening here? Seems to be an issue with units?
  # uptake[uptake > nutrients * 10000] <- nutrients * 10000 - 0.001
  uptake_exceed <- which(uptake > (nutrients * 10000))

  uptake[uptake_exceed] <- nutrients[uptake_exceed] * 10000 - 0.001

  # convert nutrient uptake to g wet biomass / tick
  # MH: What is gamma doing?
  biomass_wet <- int_convert_n(uptake, to = "g") * (gamma ^ -1)

  # calculate slough amount of blades
  # MH: In NetLogo, this part is very error prone
  blade_slough <- ifelse(biomass_wet > 0,
                         yes = biomass_wet * slough_ratio,
                         no = 0)

  # remove blade slough from growth biomass
  biomass_wet <- biomass_wet - blade_slough

  # MH: Why is there dD and d_D in NetLogo? Really confusing

  # calculate detritus amount
  # MH: Whats ag_gamma again?
  # MH: In NetLogo, this is very error prone
  detritus <- blade_slough * slough_detritus_ratio * gamma

  # add remaining nutrients to pool
  # MH: This is negative?
  # MH: Couldn't this be easier calculated using blade_slough - detritus?
  nutrients <- (-1) * int_convert_n(uptake, to = "g") +
    (1 - slough_detritus_ratio) * blade_slough * gamma

  return(list(biomass_wet = biomass_wet,
              detritus = detritus,
              nutrients = nutrients))
}
