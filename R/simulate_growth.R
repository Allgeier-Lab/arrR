#' simulate_growth
#'
#' @description Simulate movement of population.
#'
#' @param fish_population,fish_population_track Data frame population created with \code{\link{setup_fish_population}}.
#' @param seafloor RasterBrick with environment created with \code{\link{setup_seafloor}}.
#' @param parameters List with all model parameters.
#' @param min_per_i Integer to specify minutes per i.
#'
#' @details
#' Function to simulate movement of population individuals.
#'
#' @return data.frame
#'
#' @aliases simulate_growth
#' @rdname simulate_growth
#'
#' @export
simulate_growth <- function(fish_population, fish_population_track, seafloor, parameters, min_per_i) {

  # MH: Use several sub-functions

  # get number of individuals
  n <- nrow(fish_population)

  fish_population$growth_length <- parameters$pop_k_grunt * (1 / 365) * (1 / 24) * (1 / 60 ) *
    min_per_i * (parameters$pop_linf_grunt - fish_population$length)

  fish_population$growth_weight <- parameters$pop_a_grunt *
    ((fish_population$length + fish_population$growth_length) ^ parameters$pop_b_grunt -
       (fish_population$length) ^ parameters$pop_b_grunt)

  # MH: Why divided by 0.55? And why multiplied by Nbody / 100
  fish_population$consumption_req <-
    (fish_population$growth_weight + fish_population$respiration * fish_population$weight) /
    0.55 * (fish_population$n_body / 100)

  # get detritus pool at location
  detritus_pool <- raster::extract(x = seafloor$detritus_pool,
                                   y = fish_population[, c("x", "y")])

  detritus_dead <- raster::extract(x = seafloor$detritus_dead,
                                   y = fish_population[, c("x", "y")])

  wc_nutrients <- raster::extract(x = seafloor$wc_nutrients,
                                  y = fish_population[, c("x", "y")])

  # sample random ordering of individuals
  id <- sample(x = fish_population$id, size = n)

  # loop through all individuals, because one individual might use all nutrients
  for (i in id) {

    # Individuals die if the required consumption can not be met by reserves + pool
    if (fish_population$consumption_req[i] >
        (fish_population$reserves[i] + detritus_pool[i])) {

      # get starting values of individual
      indiv_starting_values <- subset(fish_population_track[[1]], id == i)

      # calculate mass difference + reserves
      mass_diff <- (fish_population$weight[i] + fish_population$reserves[i]) -
        indiv_starting_values$weight

      # add to dead detritus pool
      detritus_dead[i] <- detritus_dead[i] + mass_diff

      fish_population[i ,] <- indiv_starting_values[,-19]

      # divide starting reserves by 5 because here the formula is multiplied
      # by 0.01 and 0.05 as during setup
      reserves_wanted <- fish_population$reserves[i] / 5

      # if more reserves are wanted than available, all are used
      if (reserves_wanted >= detritus_pool[i]) {

        fish_population$reserves[i] <- detritus_pool[i]

        detritus_pool[i] <- 0

      # pool is larger than what is wanted, so only subset is used
      } else {

        fish_population$reserves[i] <- reserves_wanted

        detritus_pool[i] <- detritus_pool[i] - reserves_wanted

      }

      # increase counter died
      fish_population$died[i] <- fish_population$died[i] + 1

    # consumption requirements can be met
    } else {

      # increase age
      fish_population$age[i] <- fish_population$age[i] + min_per_i * (1 / 60) * (1 / 24)

      # individual growth
      fish_population$growth_nutrient[i] <- fish_population$growth_weight[i] *
        (fish_population$n_body[i] / 100)

      fish_population$length[i] <- fish_population$length[i] + fish_population$growth_length[i]

      fish_population$weight[i] <- fish_population$weight[i] + fish_population$growth_weight[i]

      # update reserves
      fish_population$reserves_max[i] <- 0.05 * fish_population$weight[i] * (fish_population$n_body[i] / 100)

      fish_population$reserves_diff[i] <- fish_population$reserves_max[i] - fish_population$reserves[i]

      # consumption requirement cant be meet by nutrients pool completely
      if (fish_population$consumption_req[i] <= detritus_pool[i]) {

        # calculate remaining nutrients in pool
        nutrients_left <- detritus_pool[i] - fish_population$consumption_req[i]

        # reserves can be filled completely
        if (fish_population$reserves_diff[i] <= nutrients_left) {

          # set reserves to max
          fish_population$reserves[i] <- fish_population$reserves_max[i]

          # reduce nutrient pool
          detritus_pool[i] <- nutrients_left - fish_population$reserves_diff[i]

        # reserves cannot be filled completely by nutrient pool
        } else {

          # add all nutrients that are left
          fish_population$reserves[i] <- fish_population$reserves[i] + nutrients_left

          # set pool to zero
          detritus_pool[i] <- 0

        }

      # reserves are needed to meet consumption requirement
      } else {

        # reduced reserves
        fish_population$reserves[i] <- fish_population$reserves[i] -
          (fish_population$consumption_req[i] - detritus_pool[i])

        # # set nutrient pool to 0
        detritus_pool[i] <- 0

      }
    }
  }

  egestion_nutrient <- 0 # Cn.real * FAn

  detritus_pool <- detritus_pool + egestion_nutrient

  wc_nutrients <- wc_nutrients + (fish_population$consumption_req -
                                    egestion_nutrient - fish_population$growth_nutrient)

  # get raster cells that need to be updated
  # MH: Could I do this at the beginning and not raster::extract?
  cell_id <- raster::cellFromXY(object = seafloor$detritus_pool,
                                xy = fish_population[, c("x", "y")])

  # update the detritus pool values
  raster::values(seafloor)[cell_id, c("detritus_pool",
                                      "detritus_dead",
                                      "wc_nutrients")] <- cbind(detritus_pool,
                                                                detritus_dead,
                                                                wc_nutrients)


  # raster::values(seafloor$detritus_pool)[cell_id] <- detritus_pool
  # raster::values(seafloor$detritus_dead)[cell_id] <- detritus_dead
  # raster::values(seafloor$wc_nutrients)[cell_id] <- wc_nutrients

  return(list(seafloor = seafloor, fish_population = fish_population))
}
