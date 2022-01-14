#' mdlrn_to_raster
#'
#' @description
#' Convert \code{mdl_rn} object to \code{SpatRaster}.
#'
#' @param mdl_rn \code{mdl_rn} object created with \code{\link{run_simulation}}.
#' @param verbose If TRUE, progress reports are printed.
#' @param ... Additional arguments passed on to \code{\link{rast}}.
#'
#' @details
#' Function to convert the environment (seafloor) from a previous \code{mdl_rn} object to
#' a \code{SpatRaster}. Thus, the created environment will have the final values of the
#' provided \code{mdl_rn} object as cell values. Can be used as sarting seafloor for
#' new simulation.
#'
#' @return SpatRaster
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

  reef <- mdl_rn$coords_reef

  # print progress
  if (verbose) {

    message("> ...Creating seafloor with ", mdl_rn$dimensions[1], " rows x ", mdl_rn$dimensions[2], " cols...")

    if (!is.null(reef)) {

      message("> ...Creating ", nrow(reef), " artifical reef cell(s)...")

    } else {

      message("> ...No artifical reef(s) present...")

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
  seafloor <- terra::rast(seafloor_values, res = mdl_rn$grain, type = "xyz", ...)

  return(seafloor)
}
