#' translate_torus
#'
#' @description Internal function
#'
#' @param coords 2-Column matrix/data.frame with coordinates.
#' @param extent Spatial extent object of the seafloor RasterBrick.
#'
#' @details
#' Internal function to calculate size and weight of individuals.
#'
#' @return vector
#'
#' @aliases translate_torus
#' @rdname translate_torus
#'
#' @export
translate_torus <- function(coords, extent) {

  # loop in case torus translation is largeer than plot again
  while (any(c(coords[, 1] < extent[1], coords[, 1] > extent[2],
               coords[, 2] < extent[3], coords[, 2] > extent[4]))) {

    # torus edge correction at boundaries
    coords[which(coords[, 1] < extent[1]), 1] <- extent[2] -
      (extent[1] - coords[which(coords[, 1] < extent[1]), 1])

    coords[which(coords[, 1] > extent[2]), 1] <- extent[1] +
      (coords[which(coords[, 1] > extent[2]), 1] - extent[2])

    coords[which(coords[, 2] < extent[3]), 2] <- extent[4] -
      (extent[3] - coords[which(coords[, 2] < extent[3]), 2])

    coords[which(coords[, 2] > extent[4]), 2] <- extent[3] +
      (coords[which(coords[, 2] > extent[4]), 2] - extent[4])

  }

  return(coords)
}
