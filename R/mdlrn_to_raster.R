#' mdlrn_to_raster
#'
#' @description
#' Convert mdl_rn object to SpatRaster.
#'
#' @param mdl_rn mdl_rn object.
#' @param verbose If TRUE, progress reports are printed.
#' @param ... Additional arguments passed on to \code{\link{rast}}.
#'
#' @details
#' Function to convert the seafloor from a previous \code{mdl_rn} object to
#' a \code{SpatRaster}. Thus, the created seafloor will have the final values of the
#' provided \code{mdl_rn} object as cell values.
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

  # remove time step and burn_in column
  id_remove <- which(names(mdl_rn$seafloor) %in% c("timestep", "burn_in"))

  # get selected last time step and remove time step and burnin col
  seafloor_values <- mdl_rn$seafloor[mdl_rn$seafloor$timestep == mdl_rn$max_i, -id_remove]

  # reset all tracking cols
  seafloor_values[, c("ag_production", "bg_production",
                      "ag_slough", "bg_slough",
                      "ag_uptake", "bg_uptake",
                      "consumption", "excretion")] <- 0.0

  # convert to raster
  seafloor <- terra::rast(x = seafloor_values, crs = "", type = "xyz", ...)

  return(seafloor)
}
