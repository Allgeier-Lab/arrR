#' seagrass_growth
#'
#' @description Internal function to simulate above ground seagrass
#'
#' @param nutrients Add info about parameter.
#' @param gamma Add info about parameter.
#' @param detritus_ratio Add info about parameter.
#' @param detritus_decomposition Add info about parameter.
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
#' @aliases int_seagrass_growth
#' @rdname int_seagrass_growth
#'
#' @keywords internal
#'
#' @export
int_seagrass_growth <- function(nutrients, gamma, detritus_ratio, detritus_decomposition) {

  # convert nutrient uptake to biomass growth
  biomass <- nutrients / gamma # (gamma ^ -1)

  # calculate detritus biomass
  detritus <- biomass * detritus_ratio

  # remove blade slough biomass from growth biomass
  biomass <- biomass - detritus

  # calculate detritus nutrients that go directly into nutrient pool again
  decomposition <- (detritus * gamma) * detritus_decomposition

  # calculate decomposition of detritus into nutrients pool
  nutrients <- nutrients - decomposition

  # remove decomposed detritus nutrients
  detritus <- (detritus * gamma) - decomposition

  return(list(biomass = biomass, detritus = detritus, nutrients = nutrients))
}
