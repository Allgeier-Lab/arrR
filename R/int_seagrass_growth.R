#' seagrass_growth
#'
#' @description Internal function to simulate above ground seagrass
#'
#' @param biomass_growth Add info about parameter.
#' @param biomass_reduction Add info about parameter.
#' @param nutrients Add info about parameter.
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
int_seagrass_growth <- function(biomass_growth, biomass_reduction, nutrients,
                                growth_fraction, reduction_fraction,
                                gamma, slough_ratio, slough_detritus_ratio, reduction) {

  # divide by N%/g to calculate biomass growth from nutrients
  growth <- (nutrients * growth_fraction) / gamma

  # calculate slough amount growth
  blade_slough <- ifelse(growth > 0,
                         yes = growth * slough_ratio,
                         no = 0)

  # remove blade slough from growth biomass
  growth <- growth - blade_slough

  # calculate detritus amount
  detritus <- blade_slough * slough_detritus_ratio * gamma

  # remove nutrients from wc pool reduced by slough that not goes into detritus pools
  nutrients <- -nutrients + (blade_slough * gamma - detritus)

  # calculate reduction biomass
  reduction <- biomass_reduction * (reduction_fraction * reduction)

  return(list(growth = growth, reduction = reduction,
              detritus = detritus, nutrients = nutrients))
}
