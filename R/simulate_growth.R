#' simulate_growth
#'
#' @description Simulate movement of population.
#'
#' @param population Data frame population created with \code{\link{setup_population}}.
#' @param seafloor RasterBrick with environment created with \code{\link{setup_seafloor}}.
#' @param k_grunt,linf_grunt,a_grunt,b_grunt Numeric growth parameter.
#' @param min_per_i Integer to specify minutes per i.
#' @param verbose If TRUE, progress reports are printed.
#'
#' @details
#' Function to simulate movement of population individuals.
#'
#' @return data.table
#'
#' @aliases simulate_growth
#' @rdname simulate_growth
#'
#' @export
simulate_growth <- function(population, seafloor,
                            k_grunt, linf_grunt, a_grunt, b_grunt,
                            min_per_i, verbose = TRUE) {

  # get number of individuals
  n <- nrow(population)

  population$growth_length <- k_grunt * (1 / 365) * (1 / 24) * (1 / 60 ) *
    min_per_i  * (linf_grunt - population$length)

  population$growth_weight <-
    a_grunt * ((population$length + population$growth_length) ^ b_grunt -
                 (population$length) ^ b_grunt)

  # MH: Why divided by 0.55? And why multiplied by Nbody / 100
  population$consumption_req <-
    (population$growth_weight + population$respiration * population$weight) /
    0.55 * (population$n_body / 100)

  # get detritial pool at location
  detrital_pool <- raster::extract(x = seafloor$detrital_pool, y = population[, c("x", "y")])

  # sample random ordering of individuals
  id <- sample(x = population$id, size = n)

  # loop through all individuals, because one individual might use all nutrients
  for (i in id) {

    # Individuals die if the required consumption can not be met by reserves + pool
    if (population$consumption_req[i] >
        (population$reserves[i] + detrital_pool[i])) {

      # create new fish and update pool

      # increase counter died
      population$died[i] <- population$died[i] + 1

    # consumption requirements can be met completely by pool
    } else {

      # individual growth
      population$growth_nutrient[i] <- population$growth_nutrient[i] *
        (population$n_body[i] / 100)

      population$length[i] <- population$length[i] + population$growth_length[i]

      population$weight[i] <- population$weight[i] + population$growth_weight[i]

      # update reserves
      population$reserves_max[i] <- 0.05 * population$weight[i] * (population$n_body[i] / 100)
      population$reserves_diff[i] <- population$reserves_max[i] - population$reserves[i]

      # consumption requirement cant be meet by nutrients pool completely
      if (population$consumption_req[i] <= detrital_pool[i]) {

        # calculate remaining nutrients in pool
        nutrients_left <- detrital_pool[i] - population$consumption_req[i]

        # reserves can be filled completely
        if (population$reserves_diff[i] <= nutrients_left) {

          # set reserves to max
          population$reserves[i] <- population$reserves_max[i]

          # reduce nutrient pool
          detrital_pool[i] <- detrital_pool[i] - nutrients_left

        # reserves cannot be filled completely by nutrient pool
        } else {

          # add all nutrients that are left
          population$reserves[i] <- population$reserves[i] + nutrients_left

          # set pool to zero
          detrital_pool[i] <- 0

        }

      # reserves are needed to meet consumption requirement
      } else {

        # reduced reserves
        population$reserves[i] <- population$reserves[i] -
          (population$consumption_req[i] - detrital_pool[i])

        # # set nutrient pool to 0
        detrital_pool[i] <- 0
      }
    }
  }

  # get raster cells that need to be updated
  cell_id <- raster::cellFromXY(object = seafloor$detrital_pool,
                                xy = population[, c("x", "y")])

  # update the detritial pool values
  seafloor$detrital_pool[cell_id] <- detrital_pool

  return(list(seafloor = seafloor, population = population))
}
