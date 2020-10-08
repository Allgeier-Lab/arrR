#' setup_envir_values
#'
#' @description Internal function
#'
#' @param seafloor Raster* object.
#' @param ag_biomass,bg_biomass,nutrients_pool Numeric with starting values.
#' @param detritus_pool Numeric with parameter for detritus fraction.
#' @param random Numeric with fraction by which starting values vary,
#' i.e. 0 = 0% variation; 1 = 100% variation
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
                                   nutrients_pool, detritus_pool, random) {

  # # calculate detritus nutrients (mean %N dry of Layman et al. 2016)
  # detritus_pool <- (ag_biomass * 0.0144 + bg_biomass * 0.0082) * detritus_ratio

  n_cells <- raster::ncell(seafloor)

  # create RasterLayer
  ag_biomass <- raster::setValues(x = seafloor,
                                  values = runif(n = n_cells,
                                                 min = ag_biomass * (1 - random),
                                                 max = ag_biomass * (1 + random)))

  bg_biomass <- raster::setValues(x = seafloor,
                                  values = runif(n = n_cells,
                                                 min = bg_biomass * (1 - random),
                                                 max = bg_biomass * (1 + random)))

  nutrients_pool <- raster::setValues(x = seafloor,
                                      values = runif(n = n_cells,
                                                     min = nutrients_pool * (1 - random),
                                                     max = nutrients_pool * (1 + random)))

  detritus_pool <- raster::setValues(x = seafloor,
                                     values = runif(n = n_cells,
                                                    min = detritus_pool * (1 - random),
                                                    max = detritus_pool * (1 + random)))

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
