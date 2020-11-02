#' get_neighbors
#'
#' @description Simulate diffusion.
#'
#' @param x RasterLayer.
#' @param direction Integer specifying if 4 or 8 neighborhood rule should be applied.
#' @param torus If TRUE RasterLayer is treated as torus.
#'
#' @details
#' Return matrix with cell ids of neighboring cells.
#'
#' @return matrix
#'
#' @aliases get_neighbors
#' @rdname get_neighbors
#'
#' @export
get_neighbors <- function(x, direction = 4, torus = FALSE) {

  # get number of cols
  n_ncol <- raster::ncol(x)

  # get total number of cells
  n_cell <- raster::ncell(x)

  # seq for each focal cell
  focal_id <- 1:n_cell

  # get 4 neighbors
  if (direction == 4) {

    neighbors <- cbind(focal = rep(focal_id, times = 4),
                       neighbor = c(focal_id + 1, focal_id + n_ncol,
                                    focal_id - 1, focal_id - n_ncol))

  # get 8 neighbors
  } else if (direction == 8) {

    neighbors <- cbind(focal = rep(focal_id, times = 4),
                       neighbor = c(focal_id + 1, focal_id + (n_ncol + 1),
                                    focal_id + n_ncol, focal_id + (n_ncol - 1),
                                    focal_id - 1, focal_id - (n_ncol + 1),
                                    focal_id - n_ncol, focal_id - (n_ncol - 1)))

  }

  # wrong direction selection
  else {

    stop("Please select either direction = 4 or direction = 8.", call. = FALSE )

  }

  # check which ids are outside extent
  remove_id <- which(neighbors[, 2] <= 0 | neighbors[, 2] > n_cell)

  # remove all rows outside extent
  if (!torus) {

    neighbors <- neighbors[-remove_id, ]

  # use the maximum number of cells to get torus ids
  } else {

    replace_id <- abs(n_cell - abs(neighbors[remove_id, 2]))

    neighbors[remove_id, 2] <- replace_id

  }

  # sort result
  neighbors <- neighbors[order(neighbors[, 1]), ]

  return(neighbors)
}
