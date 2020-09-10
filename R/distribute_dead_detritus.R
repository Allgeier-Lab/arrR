#' distribute_dead_detritus
#'
#' @description Redistribute dead detritus pool
#'
#' @param seafloor Environment created with \code{\link{setup_seafloor}}.
#' @param parameters List with all model parameters.
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
distribute_dead_detritus <- function(seafloor, parameters) {

  # get seafloor values
  seafloor_values <- raster::values(seafloor)

  # get values of both pools
  detritus_pool <- seafloor_values[, "detritus_pool"]

  detritus_dead <- seafloor_values[, "detritus_dead"]

  # redistribute decomposition value to detritus pool
  detritus_pool <- detritus_pool + (detritus_dead * parameters$detritus_death_decomp)

  detritus_dead <- detritus_dead - (detritus_dead * parameters$detritus_death_decomp)

  # update values
  seafloor_values[, c("detritus_pool", "detritus_dead")] <- cbind(detritus_pool, detritus_dead)

  # update environment RasterBrick
  raster::values(seafloor) <- seafloor_values

  return(seafloor)
}
