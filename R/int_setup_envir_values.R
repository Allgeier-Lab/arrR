#' setup_envir_values
#'
#' @description Internal function
#'
#' @param seafloor Raster* object.
#' @param ag_biomass,bg_biomass,nutrients_pool Numeric with starting values.
#' @param detritus_pool Numeric with parameter for detritus fraction.
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
                                   nutrients_pool, detritus_pool) {

  # # calculate detritus nutrients (mean %N dry of Layman et al. 2016)
  # detritus_pool <- (ag_biomass * 0.0144 + bg_biomass * 0.0082) * detritus_ratio

  # create RasterLayer
  ag_biomass <- raster::setValues(x = seafloor, values = ag_biomass)

  bg_biomass <- raster::setValues(x = seafloor, values = bg_biomass)

  nutrients_pool <- raster::setValues(x = seafloor, values = nutrients_pool)

  detritus_pool <- raster::setValues(x = seafloor, values = detritus_pool)

  detritus_dead <- raster::setValues(x = seafloor, values = 0)

  # combine to one RasterBrick
  seafloor <- raster::brick(ag_biomass, bg_biomass,
                            nutrients_pool,
                            detritus_pool, detritus_dead)

  # set names
  names(seafloor) <- c("ag_biomass", "bg_biomass",
                       "nutrients_pool",
                       "detritus_pool", "detritus_dead")

  return(seafloor)
}
