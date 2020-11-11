#' run_simulation
#'
#' @description Core function to run model.
#'
#' @param seafloor RasterBrick with environment created with \code{\link{setup_seafloor}}.
#' @param fish_population Data.frame population created with \code{\link{setup_fish_population}}.
#' @param parameters List with all model parameters.
#' @param reef_attraction If TRUE, individuals are attracted to AR.
#' @param max_i Integer with maximum number of simulation time steps.
#' @param min_per_i Integer to specify minutes per i.
#' @param save_each Numeric how often data should be saved to return.
#' @param verbose If TRUE, progress reports are printed.
#'
#' @details
#' Wrapper function to run model. Executes the following sub-processes (i) simulate_seagrass
#' (ii) distribute_detritus (iii) simulate_movement (iv) simulate_movement (v) simulate_respiration
#' (vi) simulate_growth (vii) simulate_mortality and (viii) simulate_diffusion.
#'
#' @return mdl_rn
#'
#' @examples
#' # Add example code
#'
#' @aliases run_simulation
#' @rdname run_simulation
#'
#' @export
run_simulation <- function(seafloor, fish_population,
                           parameters, reef_attraction,
                           max_i, min_per_i, save_each = 1,
                           verbose = TRUE) {

  # check parameters
  param_warnings <- tryCatch(check_parameters(parameters = parameters, verbose = FALSE),
                             warning = function(wrn) wrn)

  # stop with error
  if (length(param_warnings$message) > 0) {

    stop(param_warnings$message, call. = FALSE)

  }


  # check if max_i can be divided by provided save_each without reminder
  if (max_i %% save_each != 0) {

    stop("'max_i' cannot be divided by 'save_each' without rest.",
            call. = FALSE)
  }

  # check if save_each is whole number
  if (save_each %% 1 != 0) {

    stop("'save_each' must be a whole number.", call. = FALSE)

  }

  # create lists to store results for each timestep
  seafloor_track <- vector(mode = "list", length = (max_i / save_each) + 1)

  fish_population_track <- vector(mode = "list", length = (max_i / save_each) + 1)

  # convert seafloor as data.frame
  seafloor_values <- raster::as.data.frame(seafloor, xy = TRUE)

  # check if all values are within boundaries
  if (any(seafloor_values$ag_biomass < parameters$ag_biomass_min, na.rm = TRUE) |
      any(seafloor_values$bg_biomass < parameters$bg_biomass_min, na.rm = TRUE) |
      any(seafloor_values$ag_biomass > parameters$ag_biomass_max, na.rm = TRUE) |
      any(seafloor_values$bg_biomass > parameters$bg_biomass_max, na.rm = TRUE)) {

    stop("Please make sure all starting biomass values are within min/max boundaries.",
         call. = FALSE)
  }

  # get extent of environment
  extent <- raster::extent(seafloor)

  # get cell id of reef cells
  cells_reef <- raster::Which(seafloor$reef == 1,
                              cells = TRUE)

  # get coordinates of reef cells
  coords_reef <- raster::xyFromCell(object = seafloor$reef,
                                    cell = cells_reef)

  # get neighboring cells for each focal cell using torus
  cell_adj <- get_neighbors(x = seafloor, direction = 8, torus = TRUE)

  # get number of individuals
  n_pop <- nrow(fish_population)

  # save input_data as first list element
  seafloor_track[[1]] <- seafloor_values

  fish_population_track[[1]] <- fish_population

  # print some basic information about model run
  if (verbose) {

    message("> Using '", deparse(substitute(parameters)), "' as parameter list.")

    message("> Seafloor with ", extent, "; ", nrow(coords_reef), " reef cells.")

    message("> Population with ", n_pop, " individuals.")

    message("> Simulating ", max_i, " simulation iterations; Saving every ", save_each, " iterations.")

    message("> One simulation iteration equals ", min_per_i, " minutes.")

    message("")

    message("> ...Starting simulation...")

  }

  # simulate until max_i is reached
  for (i in 1:max_i) {

    if (verbose) {

      message("\r> ...Progress: ", i, "/", max_i, " simulations runs... \t\t\t",
              appendLF = FALSE)

    }

    # simulate seagrass growth
    seafloor_values <- simulate_seagrass(seafloor_values = seafloor_values,
                                         parameters = parameters,
                                         cells_reef = cells_reef,
                                         min_per_i = min_per_i)

    # redistribute detritus
    seafloor_values <- distribute_detritus(seafloor_values = seafloor_values,
                                           parameters = parameters)

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

    # diffuse values between neighbors
    seafloor_values <- simulate_diffusion(seafloor_values = seafloor_values,
                                          cell_adj = cell_adj,
                                          parameters = parameters)

    # update tracking data.frames
    if (i %% save_each == 0) {

      seafloor_track[[i / save_each + 1]] <- seafloor_values

      fish_population_track[[i / save_each + 1]] <- fish_population

    }
  }

  # new line after last progress message
  if (verbose) {

    message("")

    message("> ...Saving results...")

  }

  # combine seafloor to one dataframe
  seafloor_track <- do.call(what = "rbind", args = seafloor_track)

  # add timestep counter
  seafloor_track$timestep <- rep(x = seq(from = 0, to = max_i, by = save_each),
                                 each = raster::ncell(seafloor))

  # combine fish population to one dataframe
  fish_population_track <- do.call(what = "rbind", args = fish_population_track)

  # add timestep counter
  if (n_pop > 0) {

    fish_population_track$timestep <- rep(x = seq(from = 0, to = max_i, by = save_each),
                                          each = n_pop)

  } else {

    fish_population_track$timestep <- numeric(0)

  }

  # combine result to list
  result <- list(seafloor = seafloor_track, fish_population = fish_population_track,
                 max_i = max_i, min_per_i = min_per_i, save_each = save_each,
                 extent = extent, grain = raster::res(seafloor))

  # set class of result
  class(result) <- "mdl_rn"

  # new line after last progress message
  if (verbose) {

    message("")

    message("> All done.")

  }

  return(result)
}
