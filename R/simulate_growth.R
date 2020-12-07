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

  # randomly shuffle fish id
  fish_id <- sample(x = seq(from = 1, to = n_pop), size = n_pop)

  # get cell id of fish
  cell_id <- raster::cellFromXY(object = seafloor,
                                xy = fish_population[fish_id, c("x", "y")])

  # loop through all individuals, because one individual might use all nutrients
  for (i in seq_along(fish_id)) {

    # create counter for temp fish id
    fish_id_temp <- fish_id[i]

    # create counter for temp cell id
    cell_id_temp <- cell_id[i]

    # calculate growth in length and weight
    fish_population$growth_length[fish_id_temp] <- parameters$pop_k_grunt *
      (1 / 365) * (1 / 24) * (1 / 60) * min_per_i *
      (parameters$pop_linf_grunt - fish_population$length[fish_id_temp])

    fish_population$growth_weight[fish_id_temp] <- parameters$pop_a_grunt *
      ((fish_population$length[fish_id_temp] + fish_population$growth_length[fish_id_temp]) ^ parameters$pop_b_grunt -
         fish_population$length[fish_id_temp] ^ parameters$pop_b_grunt)

    # calculate consumption requirements
    fish_population$consumption_req[fish_id_temp] <-
      ((fish_population$growth_weight[fish_id_temp] + fish_population$respiration[fish_id_temp] * fish_population$weight[fish_id_temp]) /
         0.55) * parameters$pop_n_body

    # // calculate amount of available resources
    available_resources <-  (seafloor_values$detritus_pool[cell_id_temp] + fish_population$reserves[fish_id_temp])

    # Individuals die if the required consumption can not be met by reserves + pool
    if (fish_population$consumption_req[fish_id_temp] > available_resources) {

      # create new individual
      fish_pop_temp <- create_rebirth(fish_population = fish_population[fish_id_temp, ],
                                      fish_population_track = fish_population_track[[1]],
                                      n_body = parameters$pop_n_body,
                                      want_reserves = parameters$pop_want_reserves,
                                      detritus_pool = seafloor_values$detritus_pool[cell_id_temp],
                                      detritus_dead = seafloor_values$detritus_dead[cell_id_temp],
                                      reason = "consumption")

      # update data frames
      fish_population[fish_id_temp, ] <- fish_pop_temp$fish_population

      # update detritus
      seafloor_values$detritus_pool[cell_id_temp] <- fish_pop_temp$detritus_pool

      seafloor_values$detritus_dead[cell_id_temp] <- fish_pop_temp$detritus_dead

    # consumption requirements can be met
    } else {

      # increase age (60 min * 24 h = 1440 min/day)
      fish_population$age[fish_id_temp] <- fish_population$age[fish_id_temp] + min_per_i / 1440

      # individual growth
      fish_population$growth_nutrient[fish_id_temp] <- fish_population$growth_weight[fish_id_temp] *
        parameters$pop_n_body

      fish_population$length[fish_id_temp] <- fish_population$length[fish_id_temp] +
        fish_population$growth_length[fish_id_temp]

      fish_population$weight[fish_id_temp] <- fish_population$weight[fish_id_temp] +
        fish_population$growth_weight[fish_id_temp]

      # update reserves
      fish_population$reserves_max[fish_id_temp] <- fish_population$weight[fish_id_temp] *
        parameters$pop_n_body * parameters$pop_max_reserves

      fish_population$reserves_diff[fish_id_temp] <- fish_population$reserves_max[fish_id_temp] -
        fish_population$reserves[fish_id_temp]

      # consumption requirement cant be meet completely by nutrients pool
      if (fish_population$consumption_req[fish_id_temp] <= seafloor_values$detritus_pool[cell_id_temp]) {

        # calculate remaining nutrients in pool
        nutrients_left <- seafloor_values$detritus_pool[cell_id_temp] - fish_population$consumption_req[fish_id_temp]

        # reserves can be filled completely
        if (fish_population$reserves_diff[fish_id_temp] <= nutrients_left) {

          # save consumption
          seafloor_values$consumption[cell_id_temp] <- seafloor_values$consumption[cell_id_temp] +
            (fish_population$consumption_req[fish_id_temp] + fish_population$reserves_diff[fish_id_temp])

          # set reserves to max
          fish_population$reserves[fish_id_temp] <- fish_population$reserves_max[fish_id_temp]

          # reduce nutrient pool
          seafloor_values$detritus_pool[cell_id_temp] <- nutrients_left - fish_population$reserves_diff[fish_id_temp]

        # reserves cannot be filled completely by nutrient pool
        } else {

          # save consumption
          seafloor_values$consumption[cell_id_temp] <- seafloor_values$consumption[cell_id_temp] +
            (fish_population$consumption_req[fish_id_temp] + nutrients_left)

          # add all nutrients that are left
          fish_population$reserves[fish_id_temp] <- fish_population$reserves[fish_id_temp] + nutrients_left

          # set pool to zero
          seafloor_values$detritus_pool[cell_id_temp] <- 0

        }

      # reserves are needed to meet consumption requirement
      } else {

        # save consumption
        seafloor_values$consumption[cell_id_temp] <- seafloor_values$consumption[cell_id_temp] +
          seafloor_values$detritus_pool[cell_id_temp]

        # reduced reserves
        fish_population$reserves[fish_id_temp] <- fish_population$reserves[fish_id_temp] -
          (fish_population$consumption_req[fish_id_temp] - seafloor_values$detritus_pool[cell_id_temp])

        # set nutrient pool to 0
        seafloor_values$detritus_pool[cell_id_temp] <- 0

      }
    }

    # calc non-used consumption (excretion)
    excretion_temp <- fish_population$consumption_req[fish_id_temp] - fish_population$growth_nutrient[fish_id_temp]

    # save excretion
    seafloor_values$excretion[cell_id_temp] <- seafloor_values$excretion[cell_id_temp] + excretion_temp

    # add non-used consumption to nutrient pool (excretion)
    seafloor_values$nutrients_pool[cell_id_temp] <- seafloor_values$nutrients_pool[cell_id_temp] + excretion_temp

  }

  return(list(seafloor = seafloor_values, fish_population = fish_population))
}
