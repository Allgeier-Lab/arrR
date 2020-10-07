#' setup_seafloor
#'
#' @description Initiate environment
#'
#' @param extent Vector with number of rows and columns (spatial extent).
#' @param grain Vector with size of cells in x- and y-direction (spatial grain).
#' @param reefs 2-Column matrix with coordinates of artificial reefs.
#' @param starting_values List with all starting value parameters.
#' @param parameters List with all model parameters.
#' @param verbose If TRUE, progress reports are printed.
#' @param ... Additional arguments passed on to \code{\link{raster}}.
#'
#' @details
#' Function to setup the environment.....
#' Center of the environment is always set to (0,0).
#' All biomass values are dry values, nutrient values are g
#'
#' Parameters include ...
#'
#' @return RasterBrick
#'
#' @examples
#' reef_matrix <- matrix(data = c(-1, 0, 0, 1, 1, 0, 0, -1, 0, 0),
#' ncol = 2, byrow = TRUE)
#'
#' starting_values <- system.file("extdata", "starting_values.csv", package = "coRal")
#' parameters <- system.file("extdata", "parameters.csv", package = "coRal")
#' starting_values <- read_parameters(file = starting_values, sep = ";")
#' parameters <- read_parameters(file = parameters, sep = ";")
#'
#' input_seafloor <- setup_seafloor(extent = c(50, 50), grain = 1,
#' reefs = NULL, starting_values = starting_values, parameters = parameters)
#'
#' @aliases setup_seafloor
#' @rdname setup_seafloor
#'
#' @export
setup_seafloor <- function(extent, grain, reefs = NULL,
                           starting_values, parameters,
                           verbose = TRUE, ...) {

  # print progress
  if (verbose) {

    message("> Creating seafloor with extent(", extent[1], ", ", extent[2], ")...")

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
  seafloor <- int_setup_envir_values(seafloor = seafloor,
                                     ag_biomass = starting_values$ag_biomass,
                                     bg_biomass = starting_values$bg_biomass,
                                     nutrients_pool = starting_values$nutrients_pool,
                                     detritus_pool = starting_values$detritus_pool)

  # AR coords provided
  if (!is.null(reefs)) {

    # print progress
    if (verbose) {

      message("> Creating ", nrow(reefs), " artifical reef cells...")

    }

    # check if matrix with coords is provided
    if (!inherits(x = reefs, what = "matrix") | ncol(reefs) != 2) {

      stop("Please provide a 2-column with x,y coordinates of reef cells.",
           call. = FALSE)

    }

    # set AR = 1 and non-AR = 0 and reset environmental values to 0
    seafloor <- int_setup_reefs(object = seafloor, xy = reefs, extent = extent)

  # no AR coords provided
  } else {

    if (verbose) {

      message("> No artifical reefs present...")

    }

    # add reef layer
    seafloor$reef <- 0

  }

  return(seafloor)
}
