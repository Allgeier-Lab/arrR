#' run_simulation
#'
#' @description Wrapper functions to run model
#'
#' @param seafloor RasterBrick with environment created with \code{\link{setup_seafloor}}.
#' @param fish_population Data frame population created with \code{\link{setup_fish_population}}.
#' @param starting_values List with all starting value parameters.
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
                           starting_values, parameters, reef_attraction,
                           max_i, min_per_i = 120,
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

  # create lists to store results for each timestep
  seafloor_track <- vector(mode = "list", length = max_i)
  fish_population_track <- vector(mode = "list", length = max_i)

  # get extent of environment
  extent <- raster::extent(seafloor)

  # get coordinates of reef cells
  coords_reef <- raster::xyFromCell(object = seafloor$reef,
                                    cell = raster::Which(seafloor$reef == 1,
                                                         cells = TRUE))

  # get neighboring cells for each focal cell
  cell_adj <- raster::adjacent(x = seafloor, cells = 1:raster::ncell(seafloor),
                               directions = 8, pairs = TRUE, sort = TRUE)

  # get number of neighboring cells for each focal cell
  adj_tab <- table(cell_adj[, 1])

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
                                         coords_reef = coords_reef,
                                         reef_attraction = reef_attraction) # allow this the be change

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

    # simulate mortality
    fish_population <- simulate_mortality(fish_population = fish_population,
                                          fish_population_track = fish_population_track,
                                          seafloor = seafloor)

    # # diffuse values between neighbors (really slow at the moment)
    seafloor <- simulate_diffusion(seafloor = seafloor,
                                   cell_adj = cell_adj, adj_tab = adj_tab,
                                   parameters = parameters)

    # update tracking data.frames
    seafloor_track[[i]] <- raster::as.data.frame(seafloor, xy = TRUE)
    fish_population_track[[i]] <- fish_population

  }

  # new line after last progress message
  if (verbose) {
    message("")
  }

  # Combine to one data.frame
  seafloor_track <- do.call(what = "rbind", args = seafloor_track)
  fish_population_track <- do.call(what = "rbind", args = fish_population_track)

  # Add timestep tracker
  seafloor_track$timestep <- rep(x = 1:max_i, each = raster::ncell(seafloor))
  fish_population_track$timestep <- rep(x = 1:max_i, each = starting_values$pop_n)

  return(list(seafloor = seafloor_track, fish_population = fish_population_track))
}
