#' setup_envir_values
#'
#' @description Internal function
#'
#' @param seafloor Raster* object.
#' @param ag_biomass,bg_biomass,wc_nutrients Numeric with starting values.
#' @param detritus_fraction Numeric with parameter for detritus fraction.
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
int_setup_envir_values <- function(seafloor, ag_biomass, bg_biomass,
                                   wc_nutrients, detritus_fraction) {

  # calculate detritus nutrients (mean %N dry of Layman et al. 2016)
  detritus_pool <- (ag_biomass * 0.0144 + bg_biomass * 0.0082) * detritus_fraction

  # create RasterLayer
  ag_biomass <- raster::setValues(x = seafloor, values = ag_biomass)
  bg_biomass <- raster::setValues(x = seafloor, values = bg_biomass)
  detritus_pool <- raster::setValues(x = seafloor, values = detritus_pool)
  detritus_dead <- raster::setValues(x = seafloor, values = 0)
  wc_nutrients <- raster::setValues(x = seafloor, values = wc_nutrients)

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
