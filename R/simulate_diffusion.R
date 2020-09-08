#' simulate_diffusion
#'
#' @description Simulate diffusion.
#'
#' @param seafloor Environment created with \code{\link{setup_seafloor}}.
#' @param cell_adj 2 column matrix with cell adjacencies.
#' @param parameters List with all model parameters.
#'
#' @details
#' A certain share of each cell value is diffused to its 8 neighboring cells.
#'
#' @return RasterBrick
#'
#' @aliases simulate_diffusion
#' @rdname simulate_diffusion
#'
#' @export
simulate_diffusion <- function(seafloor, cell_adj, parameters) {

  # get id of focal cell and neighboring cell
  id_from <- cell_adj[, 1]
  id_to <- cell_adj[, 2]

  # get current values
  seafloor_values <- raster::values(seafloor)[, c("wc_nutrients", "detritus_pool",
                                                  "detritus_dead")]

  # get amount of nutrients that are diffused
  seafloor_diff <- seafloor_values %*% diag(c(parameters$wc_diffusion,
                                              parameters$detritus_diffusion,
                                              parameters$detritus_death_diffusion))

  # add diffusion values to neighboring cells
  seafloor_values[id_to, ] <- seafloor_values[id_to, ] + (seafloor_diff[id_from, ] / 8)

  # remove diffusion values from focal cell
  seafloor_values <- seafloor_values - (seafloor_diff / 8)

  # update values
  raster::values(seafloor)[, c("wc_nutrients", "detritus_pool",
                               "detritus_dead")] <- seafloor_values

  return(seafloor)
}
