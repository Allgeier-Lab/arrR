#' setup_seafloor
#'
#' @description
#' Setup seafloor for model run.
#'
#' @param dimensions Vector with number of rows and columns (spatial dimensions).
#' @param grain Vector with size of cells in x- and y-direction (spatial grain).
#' @param reef 2-Column matrix with coordinates of artificial reefs.
#' @param starting_values List with all starting value parameters.
#' @param random Numeric to randomize input values by 0 = 0 percent to 1 = 100 percent.
#' @param verbose If TRUE, progress reports are printed.
#' @param ... Additional arguments passed on to \code{\link{rast}}.
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
#' @return SpatRaster
#'
#' @examples
#' reef <- matrix(data = c(-1, 0, 0, 1, 1, 0, 0, -1, 0, 0),
#' ncol = 2, byrow = TRUE)
#'
#' seafloor <- setup_seafloor(dimensions = c(100, 100), grain = 1,
#' reef = reef, starting_values = arrR_starting_values)
#'
#' @aliases setup_seafloor
#' @rdname setup_seafloor
#'
#' @export
setup_seafloor <- function(dimensions, grain, reef = NULL, starting_values, random = 0,
                           verbose = TRUE, ...) {

  # print progress
  if (verbose) {

    message("> ...Creating seafloor with ", dimensions[1], " rows x ", dimensions[2], " cols...")

  }

  # check length of grain argument
  if (length(grain) == 1) {

    grain <- rep(x = grain, times = 2)

  } else if (length(grain) > 2) {

    stop("Please provide 'grain' argument with either one or two elements.", call. = FALSE)

  }

  # calculate extent of environment with the center being (0,0)
  extent_x <- dimensions[1] / 2 * c(-1, 1)

  extent_y <- dimensions[2] / 2 * c(-1, 1)

  layer_names <- c("ag_biomass", "bg_biomass", "nutrients_pool",
                   "detritus_pool", "detritus_fish",
                   "ag_production", "bg_production", "ag_slough", "bg_slough",
                   "ag_uptake", "bg_uptake",
                   "consumption", "excretion", "reef")

  # setup template landscape
  seafloor <- terra::rast(nrows = dimensions[1], ncol = dimensions[2], res = grain,
                          nlyrs = 14, names = layer_names,
                          xmin = extent_x[1], xmax = extent_x[2],
                          ymin = extent_y[1], ymax = extent_y[2],
                          vals = NA, crs = "", ...)

  # setup environmental values
  seafloor <- setup_envir_values(seafloor = seafloor,
                                 ag_biomass = starting_values$ag_biomass,
                                 bg_biomass = starting_values$bg_biomass,
                                 nutrients_pool = starting_values$nutrients_pool,
                                 detritus_pool = starting_values$detritus_pool,
                                 random = random)

  # AR coords provided
  if (!is.null(reef)) {

    # print progress
    if (verbose) {

      message("> ...Creating ", nrow(reef), " artifical reef cell(s)...")

    }

    # check if matrix with coords is provided
    if (!inherits(x = reef, what = "matrix")) {

      stop("Please provide a 2-column with x,y coordinates of reef cells.",
           call. = FALSE)

    }

    if (inherits(x = reef, what = "matrix") && ncol(reef) != 2) {

      stop("Please provide a 2-column with x,y coordinates of reef cells.",
           call. = FALSE)

    }

    # set AR = 1 and non-AR = 0 and reset environmental values to 0
    seafloor <- setup_reef(seafloor = seafloor, reef = reef)

  # no AR coords provided
  } else {

    if (verbose) {

      message("> ...No artifical reef(s) present...")

    }
  }

  return(seafloor)
}
