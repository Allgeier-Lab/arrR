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
#' @aliases setup_envir_values
#' @rdname setup_envir_values
#'
#' @keywords internal
#'
#' @export
int_setup_envir_values <- function(seafloor, starting_values, parameters) {

  # calculate above ground biomass
  ag_biomass <- starting_values$ag_biomass * parameters$sg_density

  # calculate below ground biomass (Layman 2016)
  # MH: Why are these values no model parameters?
  bg_biomass <- (starting_values$bg_biomass + 0.0396) / 0.0941

  # calculate detrital (Layman et al. 2016)
  detrital_pool <- (ag_biomass * parameters$ag_gamma +
                      bg_biomass * parameters$bg_gamma) * parameters$detrital_fraction

  # Value based on (Lee & Dunton 2000)
  wc_nutrients <- parameters$wc_nutrients

  # create RasterLayer
  ag_biomass <- raster::setValues(x = seafloor, values = ag_biomass)
  bg_biomass <- raster::setValues(x = seafloor, values = bg_biomass)
  detrital_pool <- raster::setValues(x = seafloor, values = detrital_pool)
  wc_nutrients <- raster::setValues(x = seafloor, values = wc_nutrients)

  # combine to one RasterBrick
  seafloor <- raster::brick(ag_biomass, bg_biomass, detrital_pool, wc_nutrients)

  # set names
  names(seafloor) <- c("ag_biomass", "bg_biomass", "detrital_pool", "wc_nutrients")

  return(seafloor)
}
