#' setup_reefs
#'
#' @description Internal function to setup reefs
#'
#' @param object Raster* object
#' @param xy 2-Column matrix with coordinates of artificial reefs
#' @param extent Vector with number of rows and columns (spatial extent).
#'
#' @details
#' Internal function to set cell values of AR = 1 and non-AR = 0. Also, sets all
#' environmental values of AR to NA/0.
#'
#' @return RasterBrick
#'
#' @examples
#' # Add example code
#'
#' @aliases setup_reefs
#' @rdname setup_reefs
#'
#' @export
setup_reefs <- function(object, xy, extent) {

  # get cell ids of provided coordinates
  cell_ids <- raster::cellFromXY(object = object, xy = xy)

  # add reef layer
  object$reef <- 0

  # add reef layer
  object$reef[cell_ids] <- 1

  # convert to matrix
  object_mat <- as.matrix(raster::as.data.frame(object$reef, xy = TRUE), ncol = 3)

  # set environmental values of AR cells to NA and 0
  raster::values(object)[cell_ids, c("ag_biomass", "bg_biomass")] <- c(NA, NA)

  return(object)
}
