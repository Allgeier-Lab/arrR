#' setup_envir_values
#'
#' @description Internal function
#'
#' @param object Raster* object
#' @param xy 2-Column matrix with coordinates of artificial reefs
#'
#' @details
#' Internal function to set biomass cell values of non-ARs cells.
#'
#' @return vector
#'
#' @aliases setup_envir_values
#' @rdname setup_envir_values
#'
#' @keywords internal
#'
#' @export
int_setup_envir_values <- function(object, starting_values, parameters) {

  # calculate aboveground biomass
  ag_biomass <- starting_values$ag_biomass * parameters$sg_density

  # calculate belowground biomass (Layman 2016)
  # MH: Why are these values no model parameters?
  bg_biomass <- (starting_values$bg_biomass + 0.0396) / 0.0941

  # calculate detrital (Layman et al. 2016)
  detrital_pool <- (ag_biomass * parameters$gamma_ag +
                      bg_biomass * parameters$gamma_bg) * parameters$detrital_fraction

  # Value based on (Lee & Dunton 2000)
  wc_nutrients <- parameters$wc_nutrients

  # create RasterLayer
  ag_biomass <- raster::setValues(x = object, values = ag_biomass)
  bg_biomass <- raster::setValues(x = object, values = bg_biomass)
  detrital_pool <- raster::setValues(x = object, values = detrital_pool)
  wc_nutrients <- raster::setValues(x = object, values = wc_nutrients)

  # combine to one RasterBrick
  object <- raster::brick(ag_biomass, bg_biomass, detrital_pool, wc_nutrients)

  # set names
  names(object) <- c("ag_biomass", "bg_biomass", "detrital_pool", "wc_nutrients")

  return(object)
}
