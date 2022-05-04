#' get_density
#'
#' @description
#' Get density of fish individuals.
#'
#' @param result mdl_rn object.
#' @param normalize Logical if TRUE, count is divided by time steps.
#' @param verbose If TRUE, progress reports are printed.
#'
#' @details
#' Calculates the fish density for each seafloor cells. Thus, the total count of
#' fish abundance within a raster cell is divided by the maximum time step.
#'
#' Please keep in mind that if not each time step was saved during \code{\link{run_simulation}},
#' the returned density might not be the "true" density, because some occurrences
#' might be missed.
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

  # create empty density data.frame
  density_df <- cbind(result$seafloor[result$seafloor$timestep == 0, c("x", "y")],
                      density = 0)

  if (nrow(result$fishpop > 0)) {

    # remove burn_in
    if (result$burn_in > 0) {

      result$fishpop <- result$fishpop[result$fishpop$burn_in == "no", ]

    }

    # convert coords to matrix
    xy_mat <- as.matrix(result$fishpop[, c("x", "y")], ncol = 2)

    # get cell id of fish population
    fish_cell <- vapply(X = 1:nrow(xy_mat), function(i) {

      rcpp_cell_from_xy(x = xy_mat[[i, "x"]], y = xy_mat[[i, "y"]],
                        extent = result$extent, dimensions = result$dimensions,
                        rcpp = FALSE)

    }, FUN.VALUE = numeric(1))

    # count number of fish within cells
    density_table <- table(fish_cell)

    # replace density of cells with fish with count
    density_df[as.numeric(names(density_table)), "density"] <- density_table

    # normalize by max_i
    if (normalize) {

      density_df$density <- density_df$density / result$max_i

    }
  }

  return(density_df)
}
