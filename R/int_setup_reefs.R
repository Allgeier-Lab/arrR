#' setup_reefs
#'
#' @description Internal function
#'
#' @param object Raster* object
#' @param xy 2-Column matrix with coordinates of artificial reefs
#' @param extent Vector with number of rows and columns (spatial extent).
#'
#' @details
#' Internal function to set cell values of AR = 1 and non-AR = 0. Also, sets all
#' environmental values of AR to 0.
#'
#' @return RasterBrick
#'
#' @aliases int_setup_reefs
#' @rdname int_setup_reefs
#'
#' @keywords internal
#'
#' @export
int_setup_reefs <- function(object, xy, extent) {

  # get cell ids of provided coordinates
  cell_ids <- raster::cellFromXY(object = object, xy = xy)

  # set environmental values of AR cells to 0
  object[cell_ids] <- 0

  # add reef layer
  object$reef <- 0

  # add reef layer
  object$reef[cell_ids] <- 1

  # conver to matrix
  object_mat <- as.matrix(raster::as.data.frame(object$reef,
                                                xy = TRUE), ncol = 3)

  # calculate distance value
  object$reef_dist <- rcpp_calc_dist_reef(seafloor = object_mat, coords_reef = xy,
                                          extent = extent, torus = TRUE)

  return(object)
}
