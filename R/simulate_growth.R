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
  fish_population$growth_length <- parameters$pop_k_grunt *
    (1 / 365) * (1 / 24) * (1 / 60 ) *
    min_per_i * (parameters$pop_linf_grunt - fish_population$length)

  fish_population$growth_weight <- parameters$pop_a_grunt *
    ((fish_population$length + fish_population$growth_length) ^ parameters$pop_b_grunt -
       fish_population$length ^ parameters$pop_b_grunt)

  # calculate consumption requirements
  fish_population$consumption_req <-
    ((fish_population$growth_weight + fish_population$respiration * fish_population$weight) /
    0.55) * parameters$pop_n_body

  # get detritus/nutrient pools at location and raster cells
  cell_id <- raster::cellFromXY(object = seafloor,
                                xy = fish_population[, c("x", "y")])

  nutrients_pool <- seafloor_values$nutrients_pool[cell_id]

  detritus_pool <- seafloor_values$detritus_pool[cell_id]

  detritus_dead <- seafloor_values$detritus_dead[cell_id]

  consumption <- seafloor_values$consumption[cell_id]

  excretion <- seafloor_values$excretion[cell_id]

  # sample random ordering of individuals
  id <- sample(x = fish_population$id, size = n_pop)

  # loop through all individuals, because one individual might use all nutrients
  for (i in id) {

    # Individuals die if the required consumption can not be met by reserves + pool
    if (fish_population$consumption_req[i] >
        (fish_population$reserves[i] + detritus_pool[i])) {

      # create new individual
      fish_pop_temp <- create_rebirth(fish_population = fish_population[i, ],
                                      fish_population_track = fish_population_track[[1]],
                                      n_body = parameters$pop_n_body,
                                      want_reserves = parameters$pop_want_reserves,
                                      detritus_pool = detritus_pool[i],
                                      detritus_dead = detritus_dead[i],
                                      reason = "consumption")

      # update data frames
      fish_population[i, ] <- fish_pop_temp$fish_population

      # update detritus
      detritus_pool[i] <- fish_pop_temp$detritus_pool

      detritus_dead[i] <- fish_pop_temp$detritus_dead

    # consumption requirements can be met
    } else {

      # increase age (60 min * 24 h = 1440 min/day)
      fish_population$age[i] <- fish_population$age[i] + min_per_i / 1440

      # individual growth
      fish_population$growth_nutrient[i] <- fish_population$growth_weight[i] *
        parameters$pop_n_body

      fish_population$length[i] <- fish_population$length[i] + fish_population$growth_length[i]

      fish_population$weight[i] <- fish_population$weight[i] + fish_population$growth_weight[i]

      # update reserves
      fish_population$reserves_max[i] <- fish_population$weight[i] * parameters$pop_n_body *
        parameters$pop_max_reserves

      fish_population$reserves_diff[i] <- fish_population$reserves_max[i] - fish_population$reserves[i]

      # consumption requirement cant be meet completely by nutrients pool
      if (fish_population$consumption_req[i] <= detritus_pool[i]) {

        # calculate remaining nutrients in pool
        nutrients_left <- detritus_pool[i] - fish_population$consumption_req[i]

        # reserves can be filled completely
        if (fish_population$reserves_diff[i] <= nutrients_left) {

          # save consumption
          consumption[i] <- consumption[i] +
            (fish_population$consumption_req[i] + fish_population$reserves_diff[i])

          # set reserves to max
          fish_population$reserves[i] <- fish_population$reserves_max[i]

          # reduce nutrient pool
          detritus_pool[i] <- nutrients_left - fish_population$reserves_diff[i]

        # reserves cannot be filled completely by nutrient pool
        } else {

         # save consumption
          consumption[i] <- consumption[i] +
            (fish_population$consumption_req[i] + nutrients_left)

          # add all nutrients that are left
          fish_population$reserves[i] <- fish_population$reserves[i] + nutrients_left

          # set pool to zero
          detritus_pool[i] <- 0

        }

      # reserves are needed to meet consumption requirement
      } else {

        # save consumption
        consumption[i] <- consumption[i] + detritus_pool[i]

        # reduced reserves
        fish_population$reserves[i] <- fish_population$reserves[i] -
          (fish_population$consumption_req[i] - detritus_pool[i])

        # set nutrient pool to 0
        detritus_pool[i] <- 0

      }
    }

    # calc non-used consumption (excretion)
    excretion_temp <- fish_population$consumption_req[i] - fish_population$growth_nutrient[i]

    # save excretion
    excretion[i] <- excretion[i] + excretion_temp

    # add non-used consumption to nutrient pool (excretion)
    nutrients_pool[i] <- nutrients_pool[i] + excretion_temp

  }

  # update values
  seafloor_values$nutrients_pool[cell_id] <- nutrients_pool

  seafloor_values$detritus_pool[cell_id] <- detritus_pool

  seafloor_values$detritus_dead[cell_id] <- detritus_dead

  seafloor_values$consumption[cell_id] <- consumption

  seafloor_values$excretion[cell_id] <- excretion

  return(list(seafloor = seafloor_values, fish_population = fish_population))
}
