#' mdlrn_to_raster
#'
#' @description
#' Convert \code{mdl_rn} object to \code{RasterBrick}.
#'
#' @param mdl_rn \code{mdl_rn} object created with \code{\link{run_simulation}}.
#' @param verbose If TRUE, progress reports are printed.
#' @param ... Additional arguments passed on to \code{\link{raster}}.
#'
#' @details
#' Function to convert the environment (seafloor) from a previous \code{mdl_rn} object to
#' a \code{RasterBrick}. Thus, the created environment will have the final values of the
#' provided \code{mdl_rn} object as cell values. Can be used as sarting seafloor for
#' new simulation.
#'
#' @return RasterBrick
#'
#' @examples
#' \dontrun{
#' mdlrn_to_raster(mdl_rn = result_rand)
#' }
#'
#' @aliases mdlrn_to_raster
#' @rdname mdlrn_to_raster
#'
#' @export
mdlrn_to_raster <- function(mdl_rn, verbose = TRUE, ...) {

  # check if mdl_rn is provided
  if (!inherits(x = mdl_rn, what = "mdl_rn")) {

    stop("Please provide 'mdl_rn' object.", call. = FALSE)

  }

  # get extent and reef coords
  extent <- mdl_rn$extent

  reefs <- mdl_rn$coords_reef

  # print progress
  if (verbose) {

    message("> ...Creating seafloor with extent(", extent[1], ", ", extent[2], ")...")

    if (!is.null(reefs)) {

      message("> ...Creating ", nrow(reefs), " artifical reef cells...")

    } else {

      message("> ...No artifical reefs present...")

    }
  }

  # get selected last timestep and remove timestep and burnin col
  seafloor_values <- mdl_rn$seafloor[mdl_rn$seafloor$timestep == mdl_rn$max_i, -c(18, 19)]

  # reset all tracking cols
  seafloor_values[, c("ag_production", "bg_production",
                      "ag_slough", "bg_slough",
                      "ag_uptake", "bg_uptake",
                      "consumption", "excretion")] <- 0

  # convert to raster
  seafloor <- raster::rasterFromXYZ(seafloor_values,
                                    res = mdl_rn$grain, ...)

  return(seafloor)
}
