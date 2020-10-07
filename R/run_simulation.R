#' run_simulation
#'
#' @description Wrapper functions to run model
#'
#' @param seafloor RasterBrick with environment created with \code{\link{setup_seafloor}}.
#' @param fish_population Data frame population created with \code{\link{setup_fish_population}}.
#' @param parameters List with all model parameters.
#' @param reef_attraction If TRUE, individuals are attracted to AR.
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
                           parameters, reef_attraction,
                           max_i, min_per_i,
                           verbose = TRUE) {

  # create lists to store results for each timestep
  seafloor_track <- vector(mode = "list", length = max_i + 1)

  fish_population_track <- vector(mode = "list", length = max_i + 1)

  # convert seafloor as data.frame
  seafloor_values <- raster::as.data.frame(seafloor, xy = TRUE)

  # add starting conditions to track lists
  seafloor_track[[1]] <- seafloor_values

  fish_population_track[[1]] <- fish_population

  # get extent of environment
  extent <- raster::extent(seafloor)

  # get cell id of reef cells
  cells_reef <- raster::Which(seafloor$reef == 1,
                              cells = TRUE)

  # get coordinates of reef cells
  coords_reef <- raster::xyFromCell(object = seafloor$reef,
                                    cell = cells_reef)

  # get neighboring cells for each focal cell using torus
  cell_adj <- int_get_neighbors(x = seafloor, direction = 8, torus = TRUE)

  # get number of individiuals
  n_pop <- nrow(fish_population)

  # # init counter for days
  # counter_day <- 0

  # print some basic information about model run
  if (verbose) {

    message("> Using '", deparse(substitute(parameters)), "' as parameter list")

    message("> Seafloor with ", extent, "; ", nrow(coords_reef), " reef cells.")

    message("> Population with ", n_pop, " individuals.")

    message("> One simulation iteration equals ", min_per_i, " minutes.")

    message("")

    message("> ...Starting simulation...")

  }

  # simulate until max_i is reached
  for (i in 1:max_i) {

    if (verbose) {

      message("\r> Progress: ", i, "/", max_i, " simulations runs \t\t\t",
              appendLF = FALSE)

    }

    # # increase counter by minutes
    # counter_day <- counter_day + min_per_i

    # simulate fish movement
    fish_population <- simulate_movement(fish_population = fish_population,
                                         n_pop = n_pop,
                                         seafloor = seafloor$reef,
                                         seafloor_values = seafloor_values,
                                         coords_reef = coords_reef,
                                         extent = extent,
                                         parameters = parameters,
                                         reef_attraction = reef_attraction)

    # simulate fish respiration (26°C is mean water temperature in the Bahamas)
    fish_population <- simulate_respiration(fish_population = fish_population,
                                            parameters = parameters,
                                            water_temp = 26,
                                            min_per_i = min_per_i)

    # simulate growth and nutrient feedback (returns population data.frame and raster)
    growth_temp <- simulate_growth(fish_population = fish_population,
                                   fish_population_track = fish_population_track,
                                   n_pop = n_pop,
                                   seafloor = seafloor$reef,
                                   seafloor_values = seafloor_values,
                                   parameters = parameters,
                                   min_per_i = min_per_i)

    # update results
    seafloor_values <- growth_temp$seafloor

    fish_population <- growth_temp$fish_population

    # simulate mortality
    mortality_temp <- simulate_mortality(fish_population = fish_population,
                                         fish_population_track = fish_population_track,
                                         n_pop = n_pop,
                                         seafloor = seafloor$reef,
                                         seafloor_values = seafloor_values,
                                         parameters = parameters,
                                         min_per_i = min_per_i)

    # update results
    seafloor_values <- mortality_temp$seafloor

    fish_population <- mortality_temp$fish_population

    # # run seagrass procedures once a day (60 min * 24 h = 1440 min/day)
    # if (counter_day == 1440) {

    # # reset counter day
    # counter_day <- 0

    # simulate seagrass growth
    seafloor_values <- simulate_seagrass(seafloor_values = seafloor_values,
                                         parameters = parameters,
                                         cells_reef = cells_reef,
                                         min_per_i = min_per_i)

    # MH: Does this make sense here in terms of scheduling?
    seafloor_values <- distribute_detritus(seafloor_values = seafloor_values,
                                           parameters = parameters)

    # diffuse values between neighbors
    seafloor_values <- simulate_diffusion(seafloor_values = seafloor_values,
                                          cell_adj = cell_adj,
                                          parameters = parameters)

    # }

    # update tracking data.frames
    seafloor_track[[i + 1]] <- seafloor_values
    fish_population_track[[i + 1]] <- fish_population

  }

  # new line after last progress message
  if (verbose) {

    message("")

  }

  # Combine to one data.frame
  seafloor_track <- do.call(what = "rbind", args = seafloor_track)

  fish_population_track <- do.call(what = "rbind", args = fish_population_track)

  # Add timestep tracker
  seafloor_track$timestep <- rep(x = 0:max_i, each = raster::ncell(seafloor))

  fish_population_track$timestep <- rep(x = 0:max_i, each = n_pop)

  # combine result to list
  result <- list(seafloor = seafloor_track, fish_population = fish_population_track,
                 max_i = max_i, min_per_i = min_per_i,
                 extent = extent, grain = raster::res(seafloor))

  # set class of result
  class(result) <- "mdl_rn"

  return(result)
}
