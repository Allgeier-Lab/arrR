#' simulate_growth
#'
#' @description Simulate consumption of fish population.
#'
#' @param fishpop_values,fishpop_track Matrix with fish population.
#' @param pop_n Numeric with number of individuals.
#' @param seafloor,seafloor_values RasterBrick and matrix with seafloor values.
#' @param parameters List with all model parameters.
#' @param min_per_i Integer to specify minutes per i.
#'
#' @details
#' Function to simulate consumption, possible mortality, growth and excretion of fish
#' population.
#'
#' @return list
#'
#' @aliases simulate_growth
#' @rdname simulate_growth
#'
#' @export
simulate_growth <- function(fishpop_values, fishpop_track, pop_n,
                            seafloor, seafloor_values, parameters, min_per_i) {

  # randomize order of loop because detritus pool can "run out"
  fish_id <- sample(x = seq(from = 1, to = pop_n), size = pop_n)

  # get detritus/nutrient pools at location and raster cells
  cell_id <- raster::cellFromXY(object = seafloor,
                                xy = fishpop_values[fish_id, c("x", "y"), drop = FALSE])

  rcpp_calc_fishpop_growth(fishpop = fishpop_values,
                           fishpop_track = fishpop_track,
                           seafloor = seafloor_values,
                           fish_id = fish_id, cell_id = cell_id,
                           pop_k = parameters$pop_k,
                           pop_linf = parameters$pop_linf,
                           pop_a = parameters$pop_a,
                           pop_b = parameters$pop_b,
                           pop_n_body = parameters$pop_n_body,
                           pop_max_reserves = parameters$pop_max_reserves,
                           pop_want_reserves = parameters$pop_want_reserves,
                           min_per_i = min_per_i)

}
