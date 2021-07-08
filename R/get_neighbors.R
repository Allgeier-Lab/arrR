#' get_neighbors
#'
#' @description
#' Get ID of neighboring cells.
#'
#' @param x RasterLayer.
#' @param direction Integer specifying if 4 or 8 neighborhood rule should be applied.
#' @param cpp Logical specifying if all indices start with 0 according to C++ indexing.
#'
#' @details
#' Internal function to get matrix with cell IDs of all neighboring cells.
#' The \code{direction} arguments allows to specifiy which cells are considered to be
#' neighbors (direction = 4: "Rook's case"; direction = 8: "Queen's case").
#'
#' @return matrix
#'
#' @examples
#' \dontrun{
#' get_neighbors(seafloor)
#' }
#'
#' @aliases get_neighbors
#' @rdname get_neighbors
#'
#' @export
get_neighbors <- function(x, direction = 8, cpp = FALSE) {

  # get total number of cells
  n_cell <- raster::ncell(x)

  # get number of cols
  n_col <- raster::ncol(x)

  # seq for each focal cell
  focal_id <- 0:(n_cell - 1)

  # get id of left, top, right, bottom id
  left <- (focal_id - 1) %% n_cell

  top <- (focal_id - n_col) %% n_cell

  right <- (focal_id + 1) %% n_cell

  bottom <- (focal_id + n_col) %% n_cell

  # get 4 neighbors
  if (direction == 4) {

    neighbors <- cbind(focal = rep(focal_id, times = 4),
                       neighbor = c(left, top, right, bottom))

    # get 8 neighbors
  } else if (direction == 8) {

    top_left <- (focal_id - (n_col + 1)) %% n_cell

    top_right <- (focal_id - (n_col - 1)) %% n_cell

    bottom_right <- (focal_id + (n_col + 1)) %% n_cell

    bottom_left <- (focal_id + (n_col - 1)) %% n_cell

    neighbors <- cbind(focal = rep(focal_id, times = 8),
                       neighbor = c(left, top_left, top, top_right,
                                    right, bottom_right, bottom, bottom_left))

  }

  # wrong direction selection
  else {

    stop("Please select either direction = 4 or direction = 8.", call. = FALSE )

  }

  # sort result
  neighbors <- neighbors[order(neighbors[, 1]), ]

  if (!cpp) {

    neighbors = neighbors + 1

  }

  return(neighbors)
}
