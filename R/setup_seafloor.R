#' setup_seafloor
#'
#' @description
#' Setup seafloor for model run.
#'
#' @param extent Vector with number of rows and columns (spatial extent).
#' @param grain Vector with size of cells in x- and y-direction (spatial grain).
#' @param reefs 2-Column matrix with coordinates of artificial reefs.
#' @param starting_values List with all starting value parameters.
#' @param random Numeric to randomize input values by 0 = 0 percent to 1 = 100 percent.
#' @param verbose If TRUE, progress reports are printed.
#' @param ... Additional arguments passed on to \code{\link{raster}}.
#'
#' @details
#' Function to setup the environment (seafloor). The center of the environment is
#' always set to (0,0). bg_biomass and ag_biomass values are in g dry weight.
#' Nutrients_pool and deritus_pool values are in g nutrients. Reef cells are indicated
#' by \code{reef = 1}, whereas non-reef cells are indicated by \code{reef = 0}. All
#' other values are increased cumulative during the model run started by \code{\link{run_simulation}}.
#'
#' If \code{random > 0}, the stochasticity is added to all starting values using \code{random}
#' as \code{x * (1 +- random)} as minimum and maximum values, respectively.
#'
#' @return RasterBrick
#'
#' @examples
#' reefs <- matrix(data = c(-1, 0, 0, 1, 1, 0, 0, -1, 0, 0),
#' ncol = 2, byrow = TRUE)
#'
#' seafloor <- setup_seafloor(extent = c(100, 100), grain = 1,
#' reefs = reefs, starting_values = default_starting_values)
#'
#' @aliases setup_seafloor
#' @rdname setup_seafloor
#'
#' @export
setup_seafloor <- function(extent, grain, reefs = NULL, starting_values, random = 0,
                           verbose = TRUE, ...) {

  # print progress
  if (verbose) {

    message("> ...Creating seafloor with extent(", extent[1], ", ", extent[2], ")...")

  }

  # calculate extent of environment with the center being (0,0)
  extent_x <- extent[1] / 2 * c(-1, 1)

  extent_y <- extent[2] / 2 * c(-1, 1)

  # setup template landscape
  seafloor <- raster::raster(nrows = extent[1], ncol = extent[2], res = grain,
                             xmn = extent_x[1], xmx = extent_x[2],
                             ymn = extent_y[1], ymx = extent_y[2],
                             vals = NA, crs = NA, ...)

  # setup environmental values
  seafloor <- setup_envir_values(seafloor = seafloor,
                                 ag_biomass = starting_values$ag_biomass,
                                 bg_biomass = starting_values$bg_biomass,
                                 nutrients_pool = starting_values$nutrients_pool,
                                 detritus_pool = starting_values$detritus_pool,
                                 random = random)

  # AR coords provided
  if (!is.null(reefs)) {

    # print progress
    if (verbose) {

      message("> ...Creating ", nrow(reefs), " artifical reef cells...")

    }

    # check if matrix with coords is provided
    if (!inherits(x = reefs, what = "matrix")) {

      stop("Please provide a 2-column with x,y coordinates of reef cells.",
           call. = FALSE)

    }

    if (inherits(x = reefs, what = "matrix") && ncol(reefs) != 2) {

      stop("Please provide a 2-column with x,y coordinates of reef cells.",
           call. = FALSE)

    }

    # set AR = 1 and non-AR = 0 and reset environmental values to 0
    seafloor <- setup_reefs(object = seafloor, xy = reefs, extent = extent)

  # no AR coords provided
  } else {

    if (verbose) {

      message("> ...No artifical reefs present...")

    }

    # add reef layer
    seafloor$reef <- 0

  }

  return(seafloor)
}
