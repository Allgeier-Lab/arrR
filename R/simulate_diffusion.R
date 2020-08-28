#' simulate_diffusion
#'
#' @description Simulate diffusion.
#'
#' @param seafloor Environment created with \code{\link{setup_seafloor}}.
#' @param parameters List with all model parameters.
#'
#' @details
#' A certain share of each cell value is diffused to its 8 neighbouring cells.
#'
#' @return RasterBrick
#'
#' @aliases simulate_diffusion
#' @rdname simulate_diffusion
#'
#' @export
simulate_diffusion <- function(seafloor, parameters) {

  # get all cell ids
  cell_ids <- 1:raster::ncell(seafloor)

  # get amount of nutrients that are diffused
  nutrients_diff <- raster::values(seafloor$wc_nutrients) * parameters$wc_diffusion / 8
  detritus_pool_diff <- raster::values(seafloor$detritus_pool) * parameters$detritus_diffusion / 8
  # add death detritus

  # get neighboring cells for each focal cell
  neighbouring_cells <- raster::adjacent(x = seafloor, cells = cell_ids,
                                         directions = 8, pairs = TRUE, sort = TRUE)

  # loop through all cell ids
  for (i in cell_ids) {

    # get id of current neighboring cells
    neighbour_id <- neighbouring_cells[which(neighbouring_cells[, 1] == i), 2]

    # add share to cells
    seafloor$wc_nutrients[neighbour_id] <- nutrients_diff[neighbour_id]
    seafloor$detritus_pool[neighbour_id] <- detritus_pool_diff[neighbour_id]
    # add death detritus

  }

  return(seafloor)
}
