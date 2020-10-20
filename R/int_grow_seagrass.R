#' grow_seagrass
#'
#' @description Internal function to simulate above ground seagrass
#'
#' @param nutrients Add info about parameter.
#' @param gamma Add info about parameter.
#' @param detritus_ratio Add info about parameter.
#'
#' @details
#' Internal function to simulate below ground seagrass processes. This
#' includes i) ... ii) ...
#'
#' #' @references
#' DeAngelis, D.L., 1992. Dynamics of Nutrient Cycling and Food Webs. Springer
#' Netherlands, Dordrecht. https://doi.org/10.1007/978-94-011-2342-6
#'
#' @return RasterBrick
#'
#' @aliases int_grow_seagrass
#' @rdname int_grow_seagrass
#'
#' @keywords internal
#'
#' @export
int_grow_seagrass <- function(nutrients, gamma, detritus_ratio) {

  # convert nutrient uptake to biomass growth
  biomass <- nutrients / gamma

  # calculate detritus biomass
  detritus <- biomass * detritus_ratio

  # remove blade slough biomass from growth biomass
  biomass <- biomass - detritus

  # remove decomposed detritus nutrients
  detritus <- detritus * gamma

  return(list(biomass = biomass, nutrients = nutrients, detritus = detritus))
}
