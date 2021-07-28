#' setup_reefs
#'
#' @description
#' Setup reef cells for seafloor.
#'
#' @param object Raster* object
#' @param xy 2-Column matrix with coordinates of artificial reefs
#' @param dimensions Vector with number of rows and columns (spatial dimensions).
#'
#' @details
#' Internal function to setup cell values of artifice reefs (reef = 1) and non-AR (reef = 0).
#' Sets all seafloor values of AR to NA/0. Used during \code{\link{setup_seafloor}}.
#'
#' @return RasterBrick
#'
#' @aliases setup_reefs
#' @rdname setup_reefs
#'
#' @export
setup_reefs <- function(object, xy, dimensions) {

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
