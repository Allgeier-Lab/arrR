#' run_simulation
#'
#' @description Wrapper functions to run model
#'
#' @param seafloor RasterBrick with environment created with \code{\link{setup_environment}}.
#' @param population Data frame population created with \code{\link{setup_population}}.
#' @param starting_values List with all starting value parameters.
#' @param parameters List with all model parameters.
#' @param max_i Integer with maximum number of simulation time steps.
#' @param min_per_i Integer to specify minutes per i.
#' @param verbose If TRUE, progress reports are printed.
#'
#' @details
#' Wrapper function to run model. Executes the following sub-processes (i) ...
#' (ii) ...
#'
#' Parameters include ...
#'
#' @return data frame
#'
#' @examples
#' \dontrun{
#'
#' run_simulation()
#'
#' }
#'
#' @aliases run_simulation
#' @rdname run_simulation
#'
#' @export
run_simulation <- function(seafloor, population,
                           starting_values, parameters, max_i, min_per_i = 120,
                           verbose = TRUE) {

  # create one deep copy of data to not change input data.table
  population <- data.table::copy(population)

  # save first environmental values to data table
  seafloor_track <- int_as_data_table_ras(x = seafloor)
  seafloor_track[, i := 0]

  # save first population values to data table (copy)
  population_track <- data.table::copy(population)
  population_track[, i := 0]

  # get extent of environment
  extent <- raster::extent(seafloor)

  # simulate until max_i is reached
  for (i in 1:max_i) {

    if (verbose) {

      # cat("\f")
      message("Progress: ", i, "/", max_i)

    }

    seafloor <- simulate_seagrass(seafloor = seafloor,
                                  parameters = parameters,
                                  min_per_i = min_per_i,
                                  verbose = verbose)

    # MH: Missing: dead-fish-detritus

    simulate_movement(population = population,
                      mean_move = parameters$pop_mean_move,
                      extent = extent,
                      reef_attraction = FALSE, # allow this the be changed
                      verbose = verbose)

    simulate_respiration(population = population, water_temp = parameters$water_temp,
                         min_per_i = min_per_i, verbose = verbose)

    simulate_growth(population = population,
                    seafloor = seafloor,
                    k_grunt = parameters$pop_k_grunt,
                    a_grunt = parameters$pop_a_grunt,
                    b_grunt = parameters$pop_b_grunt,
                    linf_grunt = parameters$pop_linf_grunt,
                    min_per_i = min_per_i,
                    verbose = verbose)

    # update track data.tables

    seafloor_track <- int_update_i(data_current = seafloor,
                                   data_track = seafloor_track,
                                   ras = TRUE)

    population_track <- int_update_i(data_current = population,
                                     data_track = population_track,
                                     ras = FALSE)

    population[, age := age + 1]
  }

  return(list(seafloor = seafloor_track, population = population_track))
}
