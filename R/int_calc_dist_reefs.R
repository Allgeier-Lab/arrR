#' calc_dist_reefs
#'
#' @description Internal function
#'
#' @param object Raster* object
#' @param xy 2-Column matrix with coordinates of artificial reefs
#'
#' @details
#' Internal function calculate distance to reef cells.
#'
#' @return vector
#'
#' @aliases calc_dist_reefs
#' @rdname calc_dist_reefs
#'
#' @keywords internal
#'
#' @export
int_calc_dist_reefs <- function(object) {

  # convert to data.frame to get coords
  object_df <- raster::as.data.frame(object, xy = TRUE)

  # split in AR/no AR data
  reefs <- object_df[object_df$layer == 1, ]
  no_reefs <- object_df[object_df$layer != 1, ]

  dist_to_reef <- vector(mode = "numeric", length = nrow(no_reefs))

  for (i in 1:nrow(no_reefs)) {

    # get current no_reef coord
    no_reef_temp <- no_reefs[i, c(1:2)]

    # calculate distance to first reef cell
    dist_min <- sqrt((reefs[1, 1] - no_reef_temp[1]) ^ 2 +
                       (reefs[1, 2] - no_reef_temp[2]) ^ 2)

    # loop through all reef cells
    for (j in 2:nrow(reefs)) {

      # get current reef cell
      reef_temp <- reefs[j, c(1:2)]

      # calculate distance
      dist_temp <- sqrt((reef_temp[1] - no_reef_temp[1]) ^ 2 +
                          (reef_temp[2] - no_reef_temp[2]) ^ 2)

      # replace minimum distance if smaller
      if (dist_temp < dist_min) {

        dist_min <- dist_temp
      }
    }

    dist_to_reef[[i]] <- as.numeric(dist_min)

  }

  reefs$dist <- 0
  no_reefs$dist <- dist_to_reef

  raster::rasterFromXYZ(xyz = rbind(reefs, no_reefs),
                        res = raster::res(object), crs = raster::crs(object))
}
