#' seagrass_growth
#'
#' @description Internal function to simulate above ground seagrass
#'
#' @param nutrients Add info about parameter.
#' @param growth_fraction Add info about parameter.
#' @param reduction_fraction Add info about parameter.
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
#' @aliases int_seagrass_growth
#' @rdname int_seagrass_growth
#'
#' @keywords internal
#'
#' @export
int_seagrass_growth <- function(nutrients, gamma, slough_ratio) {

  # convert nutrient uptake to biomass growth
  biomass <- nutrients / gamma

  # calculate detritus
  detritus <- biomass * slough_ratio

  # remove blade slough from growth biomass
  biomass <- biomass - detritus

  return(list(biomass = biomass, detritus = detritus))
}
