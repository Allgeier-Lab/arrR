#' setup_reefs
#'
#' @description Internal function
#'
#' @param object Raster* object
#' @param xy 2-Column matrix with coordinates of artificial reefs
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
int_setup_reefs <- function(object, xy) {

  # get cell ids of provided coordinates
  cell_ids <- raster::cellFromXY(object = object, xy = xy)

  # set environmental values of AR cells to 0
  object[cell_ids] <- 0

  # add reef layer
  object$reef <- 0

  # add reef layer
  object$reef[cell_ids] <- 1

  return(object)
}
