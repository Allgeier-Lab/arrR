#' simulate_growth
#'
#' @description Simulate consumption of fish population.
#'
#' @param fishpop_values,fishpop_track Data frame population created with \code{\link{setup_fishpop_values}}.
#' @param n_pop Numeric with number of individuals.
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
simulate_growth <- function(fishpop_values, fishpop_track, n_pop,
                                seafloor, seafloor_values, parameters, min_per_i) {

  # calculate growth in length and weight
  growth_length <- parameters$pop_k_grunt * (1 / 365) * (1 / 24) * (1 / 60) * min_per_i *
    (parameters$pop_linf_grunt - fishpop_values[, "length"])

  growth_weight <- parameters$pop_a_grunt *
    ((fishpop_values[, "length"] + growth_length) ^ parameters$pop_b_grunt -
       fishpop_values[, "length"] ^ parameters$pop_b_grunt)

  # calculate consumption requirements
  consumption_req <- ((growth_weight + fishpop_values[, "respiration"] *
                         fishpop_values[, "weight"]) / 0.55) * parameters$pop_n_body

  # get detritus/nutrient pools at location and raster cells
  cell_id <- raster::cellFromXY(object = seafloor,
                                xy = fishpop_values[, c("x", "y")])

  growth_values <- cbind(consumption_req, growth_length, growth_weight)

  rcpp_calc_growth(fishpop = fishpop_values,
                   fishpop_track = fishpop_track[[1]],
                   seafloor = seafloor_values,
                   cell_id = cell_id,
                   growth_values = growth_values,
                   pop_n_body = parameters$pop_n_body,
                   pop_max_reserves = parameters$pop_max_reserves,
                   pop_want_reserves = parameters$pop_want_reserves,
                   min_per_i = min_per_i)

  return(list(seafloor = seafloor_values, fishpop_values = fishpop_values))
}
