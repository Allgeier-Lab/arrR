#' distribute_dead_detritus
#'
#' @description Redistribute dead detritus pool
#'
#' @param seafloor Environment created with \code{\link{setup_seafloor}}.
#' @param detritus_death_decomp Decomposition rate of death detritus.
#'
#' @details
#' Function to redistribute dead detritus pool to overall detritus pool
#'
#' @return RasterBrick
#'
#' @aliases distribute_dead_detritus
#' @rdname distribute_dead_detritus
#'
#' @export
distribute_dead_detritus <- function(seafloor, detritus_death_decomp) {

  # get seafloor values
  seafloor_values <- raster::values(seafloor)

  # get values of both pools
  detritus_pool <- seafloor_values[, 3]

  detritus_dead <- seafloor_values[, 4]

  # redistribute decomposition value to detritus pool
  detritus_pool <- detritus_pool + (detritus_dead * detritus_death_decomp)

  detritus_dead <- detritus_dead - (detritus_dead * detritus_death_decomp)

  # update values
  seafloor_values[, c(3, 4)] <- cbind(detritus_pool, detritus_dead)

  # update environment RasterBrick
  raster::values(seafloor) <- seafloor_values

  return(seafloor)
}
