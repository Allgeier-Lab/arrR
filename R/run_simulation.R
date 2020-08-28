#' run_simulation
#'
#' @description Wrapper functions to run model
#'
#' @param seafloor RasterBrick with environment created with \code{\link{setup_seafloor}}.
#' @param fish_population Data frame population created with \code{\link{setup_fish_population}}.
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
#' @return data.frame
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
run_simulation <- function(seafloor, fish_population,
                           starting_values, parameters, max_i, min_per_i = 120,
                           verbose = TRUE) {

  # print some basic information about model run
  if (verbose) {

    message("> Using '", deparse(substitute(parameters)), "' as parameter file; '",
            deparse(substitute(starting_values)), "' as starting values.")

    message("> Seafloor with ", raster::extent(seafloor), "; ",
            sum(raster::values(seafloor$reef)), " reef cells.")

    message("> Population with ", nrow(fish_population), " individuals.")

    message("> One simulation run equals ", min_per_i, " minutes.")

    message("")
    message("> ...Starting simulation...")

  }

  # save original environmental values to data.frame
  seafloor_track <- raster::as.data.frame(x = seafloor, xy = TRUE)
  seafloor_track$track_i <- 0

  # save original population values to data.frame
  fish_population_track <- fish_population

  # check if individuals are preset
  if (nrow(fish_population) > 0) {

    fish_population_track$track_i <- 0

  }

  # get extent of environment
  extent <- raster::extent(seafloor)

  # simulate until max_i is reached
  for (i in 1:max_i) {

    if (verbose) {

      message("\r> Progress: ", i, "/", max_i, " simulations runs \t\t\t",
              appendLF = FALSE)
    }

    # simulate seagrass growth
    seafloor <- simulate_seagrass(seafloor = seafloor,
                                  parameters = parameters,
                                  min_per_i = min_per_i)

    # MH: Missing: dead-fish-detritus

    # simulate fish movement
    fish_population <- simulate_movement(fish_population = fish_population,
                                         parameters = parameters,
                                         extent = extent,
                                         reef_attraction = FALSE) # allow this the be change

    # simulate fish respiration
    fish_population <- simulate_respiration(fish_population = fish_population,
                                            water_temp = parameters$water_temp,
                                            min_per_i = min_per_i)

    # simulate growth and nutrient feedback (returns population data.frame and raster)
    result_temp <- simulate_growth(fish_population = fish_population,
                                   fish_population_track = fish_population_track,
                                   seafloor = seafloor,
                                   parameters = parameters,
                                   min_per_i = min_per_i)

    # update results
    seafloor <- result_temp$seafloor
    fish_population <- result_temp$fish_population

    fish_population <- simulate_mortality(fish_population = fish_population,
                                          fish_population_track = fish_population_track,
                                          seafloor = seafloor)

    # # diffuse values between neighbors (really slow at the moment)
    # seafloor <- simulate_diffusion(seafloor = seafloor, parameters = parameters)

    # update tracking data.frames
    seafloor_track <- int_update_i(data_current = seafloor,
                                   data_track = seafloor_track,
                                   ras = TRUE)

    fish_population_track <- int_update_i(data_current = fish_population,
                                          data_track = fish_population_track,
                                          ras = FALSE)
  }

  # new line after last progress message
  if (verbose) {
    message("")
  }

  return(list(seafloor = seafloor_track, fish_population = fish_population_track))
}
