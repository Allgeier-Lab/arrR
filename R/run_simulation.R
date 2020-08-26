#' run_simulation
#'
#' @description Wrapper functions to run model
#'
#' @param environment RasterBrick with environment created with \code{\link{setup_environment}}.
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
run_simulation <- function(environment, population,
                           starting_values, parameters, max_i, min_per_i = 120,
                           verbose = TRUE) {

  # save first environmental values to data table
  environment_track <- int_as_data_table_ras(x = environment)
  environment_track[, i := 0]

  population_track <- population
  population_track[, i := 0]

  # get extent of environment
  extent <- raster::extent(environment)

  # MH: make update i pop similar that population dt ist only current one and pop_2 is growing

  for (i in 1:max_i) {

    if (verbose) {

      cat("\f")

      message("Progress: ", i, "/", max_i)

    }

    environment <- simulate_seagrass(environment = environment,
                                     starting_values = starting_values,
                                     parameters = parameters,
                                     min_per_i = min_per_i,
                                     verbose = verbose)

    # MH: Missing: dead-fish-detritus


    population <- simulate_movement(environment = environment, population = population,
                                    mean_move = parameters$pop_mean_move,
                                    extent = extent,
                                    reef_attraction = FALSE, # allow this the be changed
                                    verbose = verbose)


    environment_track <- int_update_i(data_current = environment,
                                      data_track = environment_track,
                                      ras = TRUE)

    population_track <- int_update_i(data_current = population,
                                     data_track = population_track,
                                     ras = FALSE)
  }

  return(list(environment = environment_track, population = population_track))
}
