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
  environment_dt <- int_as_data_table_ras(x = environment)
  environment_dt$i <- 0

  for (i in 1:max_i) {

    if (verbose) {

      message("Progress: ", i, "/", max_i)

    }

    environment <- simulate_seagrass(environment = environment,
                                     starting_values = starting_values,
                                     parameters = parameters,
                                     verbose = verbose)

    environment_dt <- int_update_i_envir(environment = environment,
                                         environment_dt = environment_dt)
  }

  return(list(environment = environment_dt), population = population)
}
