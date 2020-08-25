#' setup_environment
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
#' input_environment <- setup_environment(extent = c(100, 100), grain = 1,
#' reefs = reef_matrix, starting_values = starting_values, parameters = parameters)
#'
#' @aliases setup_environment
#' @rdname setup_environment
#'
#' @export
setup_environment <- function(extent, grain, reefs = NULL,
                              starting_values, parameters,
                              verbose = TRUE, ...) {

  # print progress
  if (verbose) {

    message("Creating environment with extent(", extent[1], ", ", extent[2], ")...")

  }

  # calculate extent of environment with the center being (0,0)
  extent_x <- c(0 - extent[1] / 2, 0 + extent[1] / 2)
  extent_y <- c(0 - extent[2] / 2, 0 + extent[2] / 2)

  # force CRS to NA if not provided
  if (!exists("crs")) {

    crs <- NA
  }

  # setup template landscape
  environ_template <- raster::raster(nrows = extent[1], ncol = extent[2], res = grain,
                                     xmn = extent_x[1], xmx = extent_x[2],
                                     ymn = extent_y[1], ymx = extent_y[2],
                                     vals = NA, ...)

  # setup environmental values
  environ_template <- int_setup_envir_values(object = environ_template,
                                             starting_values = starting_values,
                                             parameters = parameters)

  # AR coords provided
  if (!is.null(reefs)) {

    # print progress
    if (verbose) {

      message("Creating ", nrow(reefs), " artifical reef cells...")

    }

    # check if matrix with coords is provided
    if (!inherits(x = reefs, what = "matrix") | ncol(reefs) != 2) {

      stop("Please provide a 2-column with x,y coordinates of reef cells.",
           call. = FALSE)

    }

    # set AR = 1 and non-AR = 0 and reset environmental values to 0
    environ_template <- int_setup_reefs(object = environ_template, xy = reefs)

  # no AR coords provided
  } else {

    message("No artifical reefs present...")

  }

  return(environ_template)
}
