#' setup_seafloor
#'
#' @description
#' Setup seafloor for model run.
#'
#' @param dimensions Vector with number of rows and columns (spatial dimensions).
#' @param grain Double with size of cells in x- and y-direction (spatial grain).
#' @param reef 2-Column matrix with coordinates of artificial reefs.
#' @param starting_values List with all starting value parameters.
#' @param random Numeric to randomize input values.
#' @param verbose If TRUE, progress reports are printed.
#'
#' @details
#' Function to setup the seafloor. The center of the seafloor raster is always x,y (0,0).
#' bg_biomass and ag_biomass values are in g dry weight. nutrients_pool and deritus_pool
#' values are in g nutrients. If reef cells are present, the cells in the corresponding
#' raster layer are idntified usinge the value one.
#'
#' If \code{random > 0}, the stochasticity is added to all starting values using
#' \code{x * (1 +- random)} as minimum and maximum values, respectively.
#'
#' @return data.frame
#'
#' @examples
#' reef <- matrix(data = c(-1, 0, 0, 1, 1, 0, 0, -1, 0, 0),
#' ncol = 2, byrow = TRUE)
#'
#' seafloor <- setup_seafloor(dimensions = c(50, 50), grain = 1,
#' reef = reef, starting_values = default_starting)
#'
#' @aliases setup_seafloor
#' @rdname setup_seafloor
#'
#' @export
setup_seafloor <- function(dimensions, grain = 1, reef = NULL, starting_values, random = 0.0,
                           verbose = TRUE) {

  # check length of grain argument
  if (length(grain) != 1) {

    stop("Please provide one 'grain' value.", call. = FALSE)

  }

  if (any(dimensions <= 0) || length(dimensions) != 2 || any(dimensions %% 1 != 0)) {

    stop("'dimensions must be a vector with two integer values.", call. = FALSE)

  }

  if (random < 0 || random > 1) {

    stop("'random' must be 0 <= x <= 1", call. = FALSE)

  }

  # calculate extent of environment with the center being (0,0)
  extent_x <- c(-dimensions[[1]] / 2, dimensions[[1]] / 2)

  extent_y <- c(-dimensions[[2]] / 2, dimensions[[2]] / 2)

  # calculate coordinates
  coords_x <- rep(x = seq(from = extent_x[[1]] + grain / 2, to = extent_x[[2]] - grain / 2,
                          by = grain), times = dimensions[[2]] / grain)

  coords_y <- rep(x = rev(seq(from = extent_y[[1]] + grain / 2, to = extent_y[[2]] - grain / 2,
                              by = grain)), each = dimensions[[1]] / grain)

  if (length(coords_x) != length(coords_y)) {

    stop("Length of x and y coordinates are not identical.", call. = FALSE)

  }

  # # calculate total number of cells
  n_cells <- length(coords_x)

  # create seafloor value
  ag_biomass <- stats::runif(n = n_cells, min = starting_values$ag_biomass * (1 - random),
                             max = starting_values$ag_biomass * (1 + random))

  bg_biomass <- stats::runif(n = n_cells, min = starting_values$bg_biomass * (1 - random),
                             max = starting_values$bg_biomass * (1 + random))

  nutrients_pool <- stats::runif(n = n_cells, min = starting_values$nutrients_pool * (1 - random),
                                 max = starting_values$nutrients_pool * (1 + random))

  detritus_pool <- stats::runif(n = n_cells, min = starting_values$detritus_pool * (1 - random),
                                max = starting_values$detritus_pool * (1 + random))

  # print progress
  if (verbose) {

    message("> ...Creating seafloor with ", length(unique(coords_x)), " rows x ",
            length(unique(coords_y)), " cols...")

  }


  # create data.frame
  seafloor <- data.frame(x = coords_x, y = coords_y, ag_biomass = ag_biomass, bg_biomass = bg_biomass,
                         nutrients_pool = nutrients_pool, detritus_pool = detritus_pool,
                         detritus_fish = numeric(length = n_cells), ag_production = numeric(length = n_cells),
                         bg_production = numeric(length = n_cells), ag_slough = numeric(length = n_cells),
                         bg_slough = numeric(length = n_cells), ag_uptake = numeric(length = n_cells),
                         bg_uptake = numeric(length = n_cells), consumption = numeric(length = n_cells),
                         excretion = numeric(length = n_cells), reef = numeric(length = n_cells))

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

    # get reef cell ids
    reef_id <- vapply(1:nrow(reef), function(i) {

      rcpp_cell_from_xy(x = reef[[i, 1]], y = reef[[i, 2]], extent = c(extent_x, extent_y),
                        dimensions = c(dimensions[[2]] / grain, dimensions[[1]] / grain),
                        rcpp = FALSE)

    }, FUN.VALUE = numeric(1))

    seafloor[reef_id, "reef"] <- 1

    seafloor[reef_id, c("ag_biomass", "bg_biomass", "ag_production", "bg_production",
                        "ag_slough", "bg_slough", "ag_uptake", "bg_uptake")] <- NA

    # no AR coords provided
  } else {

    if (verbose) {

      message("> ...No artifical reef(s) present...")

    }
  }

  return(seafloor)
}
