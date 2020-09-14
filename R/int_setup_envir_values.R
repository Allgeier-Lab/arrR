#' setup_envir_values
#'
#' @description Internal function
#'
#' @param seafloor Raster* object
#' @param xy 2-Column matrix with coordinates of artificial reefs
#'
#' @details
#' Internal function to set biomass cell values of non-ARs cells.
#'
#' @return vector
#'
#' @aliases int_setup_envir_values
#' @rdname int_setup_envir_values
#'
#' @keywords internal
#'
#' @export
int_setup_envir_values <- function(seafloor, starting_values, parameters) {

  # calculate below ground biomass
  # MH: Wet-Dry conversion
  bg_biomass <- (starting_values$bg_biomass + 0.0396) / 0.0941

  # calculate detritus (Layman et al. 2016)
  detritus_pool <- (starting_values$ag_biomass * parameters$ag_gamma +
                      bg_biomass * parameters$bg_gamma) * parameters$detritus_fraction

  # create RasterLayer
  ag_biomass <- raster::setValues(x = seafloor, values = starting_values$ag_biomass)
  bg_biomass <- raster::setValues(x = seafloor, values = bg_biomass)
  detritus_pool <- raster::setValues(x = seafloor, values = detritus_pool)
  detritus_dead <- raster::setValues(x = seafloor, values = 0)
  wc_nutrients <- raster::setValues(x = seafloor, values = starting_values$wc_nutrients)

  # combine to one RasterBrick
  seafloor <- raster::brick(ag_biomass, bg_biomass,
                            detritus_pool,detritus_dead = detritus_dead,
                            wc_nutrients)

  # set names
  names(seafloor) <- c("ag_biomass", "bg_biomass",
                       "detritus_pool", "detritus_dead",
                       "wc_nutrients")

  return(seafloor)
}
