#' setup_reef
#'
#' @description
#' Setup reef cells for seafloor.
#'
#' @param seafloor Raster* object
#' @param reef 2-Column matrix with coordinates of artificial reefs
#'
#' @details
#' Setup cell values of artifice reefs (reef = 1) and non-AR (reef = 0).
#' Sets all seafloor values of AR to NA/0. Used during \code{\link{setup_seafloor}}.
#'
#' @return SpatRaster
#'
#' @aliases .setup_reef
#' @rdname .setup_reef
#'
#' @keywords internal
.setup_reef <- function(seafloor, reef) {

  # get cell ids of provided coordinates
  cell_ids <- terra::cellFromXY(object = seafloor, xy = reef)

  # add reef layer
  terra::values(seafloor)[cell_ids, "reef"] <- 1

  # set environmental values of AR cells to NA and 0
  terra::values(seafloor)[cell_ids, c("ag_biomass", "bg_biomass")] <- c(NA, NA)

  return(seafloor)
}
