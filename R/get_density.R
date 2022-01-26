#' get_density
#'
#' @description
#' Get density of fish individuals within cell.
#'
#' @param result mdl_rn object of simulation run.
#' @param normalize Logical if TRUE count is divided by timesteps.
#' @param verbose If TRUE, progress reports are printed.
#'
#' @details
#' Calculates the fish density for each cells. Thus, the total count of
#' fish occurrences within a raster cell is divided by the maximum timestep. Please
#' keep in mind that if not each timestep was saved during \code{\link{run_simulation}},
#' the returned density might not be the "true" density because some occurrences might be missed.
#'
#' @return data.frame
#'
#' @examples
#' \dontrun{
#' get_density(result = result_rand)
#' }
#'
#' @aliases get_density
#' @rdname get_density
#'
#' @export
get_density <- function(result, normalize = FALSE, verbose = TRUE) {

  # check if mdl_rn is provided
  if (!inherits(x = result, what = "mdl_rn")) {

    stop("Please provide 'mdl_rn' object createt with run_simulation.", call. = FALSE)

  }

  # return warning if save_each != 1 because not all occurrences are counted
  if (result$save_each != 1) {

    if (verbose) {

      warning("Please be aware that 'true' density might be higher because 'save_each' is not one.",
              call. = FALSE)

    }
  }

  # create empty raster
  ras_density <- terra::rast(ext = terra::ext(result$extent), resolution = result$grain,
                             crs = "", vals = 0.0, names = "density")

  if (nrow(result$fishpop > 0)) {

    # remove burn_in
    if (result$burn_in > 0) {

      result$fishpop <- result$fishpop[result$fishpop$burn_in == "no", ]

    }

    # convert coords to matrix
    xy_mat <- as.matrix(result$fishpop[, c("x", "y")], ncol = 2)

    # count fish within each cell
    ras_density <- terra::rasterize(x = terra::vect(x = xy_mat, crs = ""), y = ras_density,
                                    fun = "length", background = 0)

    # normalize by max_i
    if (normalize) {

      terra::values(ras_density)[, "density"] <- terra::values(ras_density)[, "density"] /
        result$max_i

    }
  }

  # convert to dataframe
  ras_density <- terra::as.data.frame(ras_density, xy = TRUE, na.rm = FALSE)

  return(ras_density)
}
