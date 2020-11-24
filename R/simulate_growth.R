#' simulate_growth
#'
#' @description Simulate consumption of fish population.
#'
#' @param fish_population,fish_population_track Data frame population created with \code{\link{setup_fish_population}}.
#' @param n_pop Numeric with number of individuals.
#' @param seafloor,seafloor_values RasterLayer and data.frame with seafloor values.
#' @param parameters List with all model parameters.
#' @param min_per_i Integer to specify minutes per i.
#'
#' @details
#' Function to simulate consumption, possible mortality, growth and excretion of fish
#' population.
#'
#' @return data.frame
#'
#' @aliases simulate_growth
#' @rdname simulate_growth
#'
#' @export
simulate_growth <- function(fish_population, fish_population_track, n_pop,
                                seafloor, seafloor_values, parameters, min_per_i) {

  # calculate growth in length and weight
  growth_length <- parameters$pop_k_grunt * (1 / 365) * (1 / 24) * (1 / 60) * min_per_i *
    (parameters$pop_linf_grunt - fish_population$length)

  growth_weight <- parameters$pop_a_grunt *
    ((fish_population$length + growth_length) ^ parameters$pop_b_grunt -
       fish_population$length ^ parameters$pop_b_grunt)

  # calculate consumption requirements
  consumption_req <- ((fish_population$growth_weight + fish_population$respiration *
                         fish_population$weight) / 0.55) * parameters$pop_n_body

  # get detritus/nutrient pools at location and raster cells
  cell_id <- raster::cellFromXY(object = seafloor,
                                xy = fish_population[, c("x", "y")])

  # # sample random ordering of individuals
  # id <- sample(x = fish_population$id, size = n_pop)

  id_mort <- which(consumption_req > (fish_population$reserves +
                                        seafloor_values[cell_id, "detritus_pool"]))

  if (length(id_mort) > 0) {

    cell_id_temp <- cell_id[id_mort]

    # create new individual
    mort_temp <- create_rebirth(fish_population = fish_population[id_mort, ],
                                fish_population_track = fish_population_track[[1]],
                                n_body = parameters$pop_n_body,
                                want_reserves = parameters$pop_want_reserves,
                                detritus_pool = seafloor_values$detritus_pool[cell_id_temp],
                                detritus_dead = seafloor_values$detritus_dead[cell_id_temp],
                                reason = "consumption")

    # update data frames
    fish_population[id_mort, ] <- mort_temp$fish_population

    # update detritus
    seafloor_values$detritus_pool[cell_id_temp] <- mort_temp$detritus_pool

    seafloor_values$detritus_pool[cell_id_temp] <- mort_temp$detritus_dead

  }

  id_growth <- which(consumption_req <= (fish_population$reserves +
                                           seafloor_values[cell_id, "detritus_pool"]))

  if (length(id_growth) > 0) {

    cell_id_temp <- cell_id[id_growth]

    growth_values <- cbind(consumption_req, growth_length, growth_weight)

    seafloor_temp <- as.matrix(seafloor_values[cell_id_temp, c("nutrients_pool", "detritus_pool",
                                                               "detritus_dead", "consumption",
                                                               "excretion")])

    fish_temp <- as.matrix(fish_population[id_growth, c("id", "age",
                                                        "length", "weight",
                                                        "reserves", "reserves_max")])

    rcpp_calc_growth(fish_pop = fish_temp,
                     seafloor = seafloor_temp,
                     growth_values = growth_values,
                     pop_n_body = parameters$pop_n_body,
                     pop_max_reserves = parameters$pop_max_reserves,
                     min_per_i = min_per_i)

    # update data frames
    fish_population[id_growth, c("id", "age",
                                 "length", "weight",
                                 "reserves", "reserves_max")] <- fish_temp

    # update data frames
    seafloor_values[cell_id_temp, c("nutrients_pool", "detritus_pool",
                                    "detritus_dead", "consumption",
                                    "excretion")] <- seafloor_temp
  }

  return(list(seafloor = seafloor_values, fish_population = fish_population))
}
