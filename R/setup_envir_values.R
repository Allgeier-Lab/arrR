#' setup_envir_values
#'
#' @description
#' Setup environmental values for seafloor.
#'
#' @param seafloor Raster* object.
#' @param ag_biomass,bg_biomass,nutrients_pool Numeric with starting values.
#' @param detritus_pool Numeric with parameter for detritus fraction.
#' @param random Numeric to randomize input values by 0 = 0 percent to 1 = 100 percent.
#'
#' @details
#' Setup environmental values used during \code{\link{setup_seafloor}}.
#' The function creates a \code{RasterBrick} with all required values. All incremental
#' values that will be increased during model run are set to zero. The function does not
#' setup reef cells.
#'
#' If \code{random > 0}, the stochasticity is added to all starting values using \code{random}
#' as
#'
#' @return RasterBrick
#'
#' @aliases setup_envir_values
#' @rdname setup_envir_values
#'
#' @export
setup_envir_values <- function(seafloor, ag_biomass, bg_biomass,
                               nutrients_pool, detritus_pool, random) {

  # get number of cells
  n_cells <- raster::ncell(seafloor)

  # create RasterLayer
  ag_biomass <- raster::setValues(x = seafloor, values = stats::runif(n = n_cells,
                                                                      min = ag_biomass * (1 - random),
                                                                      max = ag_biomass * (1 + random)))

  bg_biomass <- raster::setValues(x = seafloor, values = stats::runif(n = n_cells,
                                                                      min = bg_biomass * (1 - random),
                                                                      max = bg_biomass * (1 + random)))

  nutrients_pool <- raster::setValues(x = seafloor, values = stats::runif(n = n_cells,
                                                                          min = nutrients_pool * (1 - random),
                                                                          max = nutrients_pool * (1 + random)))

  detritus_pool <- raster::setValues(x = seafloor, values = stats::runif(n = n_cells,
                                                                         min = detritus_pool * (1 - random),
                                                                         max = detritus_pool * (1 + random)))

  detritus_fish <- raster::setValues(x = seafloor, values = 0)

  ag_production <- raster::setValues(x = seafloor, values = 0)

  bg_production <- raster::setValues(x = seafloor, values = 0)

  ag_slough <- raster::setValues(x = seafloor, values = 0)

  bg_slough <- raster::setValues(x = seafloor, values = 0)

  ag_uptake <- raster::setValues(x = seafloor, values = 0)

  bg_uptake <- raster::setValues(x = seafloor, values = 0)

  consumption <- raster::setValues(x = seafloor, values = 0)

  excretion <- raster::setValues(x = seafloor, values = 0)

  # combine to one RasterBrick
  seafloor <- raster::brick(ag_biomass, bg_biomass, nutrients_pool,
                            detritus_pool, detritus_fish,
                            ag_production, bg_production, ag_slough, bg_slough,
                            ag_uptake, bg_uptake,
                            consumption, excretion)

  # set names
  names(seafloor) <- c("ag_biomass", "bg_biomass", "nutrients_pool",
                       "detritus_pool", "detritus_fish",
                       "ag_production", "bg_production", "ag_slough", "bg_slough",
                       "ag_uptake", "bg_uptake",
                       "consumption", "excretion")

  return(seafloor)
}
