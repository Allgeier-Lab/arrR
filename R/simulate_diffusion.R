#' simulate_diffusion
#'
#' @description Simulate diffusion.
#'
#' @param seafloor Environment created with \code{\link{setup_seafloor}}.
#' @param cell_adj 2 column matrix with cell adjacencies.
#' @param adj_tab Table with count of neighbors for each focal cell.
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
simulate_diffusion <- function(seafloor, cell_adj, adj_tab, parameters) {

  # get amount of nutrients that are diffused
  # MH: add death detritus
  wc_nutrients_diff <- raster::values(seafloor$wc_nutrients) * parameters$wc_diffusion
  detritus_pool_diff <- raster::values(seafloor$detritus_pool) * parameters$detritus_diffusion
  detritus_dead_diff <- raster::values(seafloor$detritus_dead) * parameters$detritus_death_diffusion

  # get id of focal cell and neighboring cell
  id_from <- cell_adj[, 1]
  id_to <- cell_adj[, 2]

  # add diffusion value from focal cell to neighboring cell
  seafloor$wc_nutrients[id_to] <- seafloor$wc_nutrients[id_to] +
    (wc_nutrients_diff[id_from] / 8)

  seafloor$detritus_pool[id_to] <- seafloor$detritus_pool[id_to] +
    (detritus_pool_diff[id_from] / 8)

  seafloor$detritus_dead[id_to] <- seafloor$detritus_dead[id_to] +
    (detritus_dead_diff[id_from] / 8)

  # remove diffused amount from focal cell
  seafloor$wc_nutrients[unique(id_from)] <- seafloor$wc_nutrients[unique(id_from)] -
    wc_nutrients_diff[unique(id_from)] / adj_tab

  seafloor$detritus_pool[unique(id_from)] <- seafloor$detritus_pool[unique(id_from)] -
    detritus_pool_diff[unique(id_from)] / adj_tab

  seafloor$detritus_dead[unique(id_from)] <- seafloor$detritus_dead[unique(id_from)] -
    detritus_dead_diff[unique(id_from)] / adj_tab

  # # loop through all cell ids
  # for (i in raster::ncell(seafloor)) {
  #
  #   # MH: This is the bottleneck
  #   # get id of current neighboring cells
  #   neighbour_id <- neighbouring_cells[which(neighbouring_cells[, 1] == i), 2]
  #
  #   # add share to cells
  #   seafloor$wc_nutrients[neighbour_id] <- nutrients_diff[neighbour_id]
  #   seafloor$detritus_pool[neighbour_id] <- detritus_pool_diff[neighbour_id]
  #   # add death detritus
  #
  # }

  return(seafloor)
}
