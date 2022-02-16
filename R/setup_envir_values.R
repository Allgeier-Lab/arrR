#' setup_envir_values
#'
#' @description
#' Setup environmental values for seafloor.
#'
#' @param seafloor SpatRaster object.
#' @param ag_biomass,bg_biomass,nutrients_pool Numeric with starting values.
#' @param detritus_pool Numeric with parameter for detritus fraction.
#' @param random Numeric to randomize input values.
#'
#' @details
#' Create a \code{SpatRaster} with all required values for model run. The function
#' does not setup reef cells.
#'
#' If \code{random > 0}, the stochasticity is added to all starting values using
#' \code{x * (1 +- random)} as minimum and maximum values, respectively.
#'
#' @return SpatRaster
#'
#' @aliases setup_envir_values
#' @rdname setup_envir_values
#'
#' @keywords internal
.setup_envir_values <- function(seafloor, ag_biomass, bg_biomass,
                                nutrients_pool, detritus_pool, random) {

  # get number of cells
  n_cells <- terra::ncell(seafloor)

  # set starting values standing pools
  terra::values(seafloor)[, "ag_biomass"] <- stats::runif(n = n_cells,
                                                          min = ag_biomass * (1 - random),
                                                          max = ag_biomass * (1 + random))

  terra::values(seafloor)[, "bg_biomass"] <- stats::runif(n = n_cells,
                                                          min = bg_biomass * (1 - random),
                                                          max = bg_biomass * (1 + random))

  terra::values(seafloor)[, "nutrients_pool"] <- stats::runif(n = n_cells,
                                                              min = nutrients_pool * (1 - random),
                                                              max = nutrients_pool * (1 + random))

  terra::values(seafloor)[, "detritus_pool"] <- stats::runif(n = n_cells,
                                                             min = detritus_pool * (1 - random),
                                                             max = detritus_pool * (1 + random))

  return(seafloor)
}
