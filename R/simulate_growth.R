#' simulate_growth
#'
#' @description Simulate movement of population.
#'
#' @param population Data frame population created with \code{\link{setup_population}}.
#' @param seafloor RasterBrick with environment created with \code{\link{setup_environment}}.
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

  # this is probably really bad practice...
  parent_frame <- parent.frame()

  # get number of individuals
  n <- nrow(population)

  population[, growth_length := k_grunt * (1 / 365) * (1 / 24) * (1 / 60 ) * min_per_i  * (linf_grunt - length)]

  population[, growth_weight := a_grunt * ((population$length + growth_length) ^ b_grunt - (population$length) ^ b_grunt)]

  # MH: Why divided by 0.55? And why multiplied by Nbody/100
  population[, consumption_req := (growth_weight + population$respiration * population$weight) / 0.55 * (n_body / 100)]
  # population[, consumption_req := consumption_req * (n_body / 100)]

  # get detritial pool at location
  detrital_pool <- raster::extract(x = seafloor$detrital_pool, y = population[, c("x", "y")])

  # sample random ordering of individuals
  id <- sample(x = population$id, size = n)

  # loop through all individuals, because one individual might use all nutrients
  for (i in id) {

    # Individuals die if the required consumption can not be met by reserves + pool
    if (population[i, consumption_req] >
        (population[i, reserves] + detrital_pool[i])) {

      starting_size <- parent_frame$population_track[id == i,]

      # update the detritial pool values (parental environment)
      parent_frame$seafloor$detrital_pool[cell_id] <- detrital_pool

      # let mass-difference a.grunt * (body-length-current ^ (b.grunt) - (new-size) ^ (b.grunt))

      # increase counter died
      population[i, died := died + 1]

    # consumption requirements can be met completely by pool
    } else {

      # individual growth
      population[i, growth_nutrient := growth_weight * (n_body / 100)]
      population[i, length := length + growth_length]
      population[i, weight := weight + growth_weight]

      # update reserves
      population[i, reserves_max := 0.05 * weight * (n_body / 100)]
      population[i, reserves_diff := reserves_max - reserves]

      # consumption requirement cant be meet by nutrients pool completely
      if (population[i, consumption_req] <= detrital_pool[i]) {

        # calculate remaining nutrients in pool
        nutrients_left <- detrital_pool[i] - population[i, consumption_req]

        # reserves can be filled completely
        if (population[i, reserves_diff] <= nutrients_left) {

          # set reserves to max
          population[i, reserves := reserves_max]

          # reduce nutrient pool
          detrital_pool[i] <- detrital_pool[i] - nutrients_left

        # reserves cannot be filled completely by nutrient pool
        } else {

          # see comment l95
          reserves_new <- population[i, reserves] + nutrients_left

          # add all nutrients that are left
          population[i, reserves := reserves_new]

          # set pool to zero
          detrital_pool[i] <- 0

        }

      # reserves are needed to meet consumption requirement
      } else {

        # if I do that directly in [] there is a weird BUG that sometimes I get a NA
        reserves_new <- population[i, reserves] - (population[i, consumption_req] - detrital_pool[i])

        # reduced reserves
        population[i, reserves := reserves_new]

        # # set nutrient pool to 0
        detrital_pool[i] <- 0
      }
    }
  }

  # get cell ids that need to be updated
  cell_id <- raster::cellFromXY(object = seafloor$detrital_pool,
                                xy = population[, c("x", "y")])

  # update the detritial pool values (parental environment)
  parent_frame$seafloor$detrital_pool[cell_id] <- detrital_pool
}
