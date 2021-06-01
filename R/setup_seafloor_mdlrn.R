#' setup_seafloor_mdlrn
#'
#' @description Initiate environment (seafloor) from model run
#'
#' @param mdl_rn \code{mdl_rn} object created with \code{\link{run_simulation}}.
#' @param verbose If TRUE, progress reports are printed.
#' @param ... Additional arguments passed on to \code{\link{raster}}.
#'
#' @details
#' Function to setup the environment (seafloor) from previous model run.
#'
#' @return RasterBrick
#'
#' @examples
#' # Add example code
#'
#' @aliases setup_seafloor_mdlrn
#' @rdname setup_seafloor_mdlrn
#'
#' @export
setup_seafloor_mdlrn <- function(mdl_rn, verbose = TRUE, ...) {

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
