#' simulate_mortality
#'
#' @description Simulate background mortality of population.
#'
#' @param fishpop_values,fishpop_track Data frame population created
#' with \code{\link{setup_fishpop}}.
#' @param pop_n Numeric with number of individuals.
#' @param seafloor,seafloor_values RasterBrick and matrix with seafloor values.
#' @param parameters List with all model parameters.
#' @param min_per_i Integer to specify minutes per i.
#'
#' @details
#' Function to simulate background mortality of fish population individuals.
#'
#' @return list
#'
#' @aliases simulate_mortality
#' @rdname simulate_mortality
#'
#' @export
simulate_mortality <- function(fishpop_values, fishpop_track,
                               seafloor, seafloor_values,
                               pop_n, parameters, min_per_i) {

  # randomize order of loop because detritus pool can "run out"
  fish_id <- sample(x = seq(from = 1, to = pop_n), size = pop_n)

  # get detritus/nutrient pools at location and raster cells
  cell_id <- raster::cellFromXY(object = seafloor,
                                xy = fishpop_values[fish_id, c("x", "y"),
                                                    drop = FALSE])

  # create new individual
  rcpp_calc_mortality(fishpop = fishpop_values,
                      fishpop_track = fishpop_track,
                      seafloor = seafloor_values,
                      fish_id = fish_id, cell_id = cell_id,
                      pop_linf = parameters$pop_linf,
                      pop_n_body = parameters$pop_n_body,
                      pop_want_reserves = parameters$pop_want_reserves)

}
