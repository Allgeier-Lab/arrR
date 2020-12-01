#' run_simulation
#'
#' @description Core function to run model.
#'
#' @param seafloor RasterBrick with environment created with \code{\link{setup_seafloor}}.
#' @param fishpop Data.frame with fish population created with \code{\link{setup_fishpop}}.
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
run_simulation <- function(seafloor, fishpop,
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

  # convert seafloor and fishpop as matrix
  seafloor_values <- as.matrix(raster::as.data.frame(seafloor, xy = TRUE))

  fishpop_values <- as.matrix(fishpop)

  # get mean starting values
  starting_values <- get_starting_values(seafloor_values = seafloor_values,
                                         fishpop_values = fishpop_values)

  # create data.frame to store results for each timestep
  seafloor_track <- data.frame(matrix(nrow = (max_i / save_each) * nrow(seafloor_values) +
                                        nrow(seafloor_values), ncol = ncol(seafloor_values)))

  names(seafloor_track) <- c("x", "y", names(seafloor))

  fishpop_track <- data.frame(matrix(nrow = (max_i / save_each) * nrow(fishpop_values) +
                                       nrow(fishpop_values), ncol = ncol(fishpop_values)))

  names(fishpop_track) <- names(fishpop)

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

  # save input data in tracking data.frame
  seafloor_track[1:nrow(seafloor_values), ] <- seafloor_values

  fishpop_track[1:nrow(fishpop_values), ] <- fishpop_values

  # print some basic information about model run
  if (verbose) {

    message("> Seafloor with ", extent, "; ", nrow(coords_reef), " reef cells.")

    message("> Population with ", starting_values$pop_n, " individuals.")

    message("> Simulating ", max_i, " simulation iterations; Saving every ", save_each, " iterations.")

    message("> One simulation iteration equals ", min_per_i, " minutes.")

    message("")

    message("> ...Starting simulation...")

  }

  # simulate until max_i is reached
  for (i in 1:max_i) {

    # simulate seagrass growth
    simulate_seagrass(seafloor_values = seafloor_values,
                      parameters = parameters,
                      cells_reef = cells_reef,
                      min_per_i = min_per_i)

    # redistribute detritus
    simulate_mineralization(seafloor_values = seafloor_values,
                            parameters = parameters)

    # simulate fish movement
    simulate_movement(fishpop_values = fishpop_values,
                      pop_n = starting_values$pop_n,
                      seafloor = seafloor$reef,
                      seafloor_values = seafloor_values,
                      coords_reef = coords_reef,
                      extent = extent,
                      parameters = parameters,
                      reef_attraction = reef_attraction)

    # simulate fish respiration (26°C is mean water temperature in the Bahamas)
    simulate_respiration(fishpop_values = fishpop_values,
                         parameters = parameters,
                         water_temp = 26,
                         min_per_i = min_per_i)

    # simulate fishpop growth and including change of seafloor pools
    simulate_fishpop_growth(fishpop_values = fishpop_values,
                            fishpop_track = as.matrix(fishpop_track[1:starting_values$pop_n, ]),
                            pop_n = starting_values$pop_n,
                            seafloor = seafloor$reef,
                            seafloor_values = seafloor_values,
                            parameters = parameters,
                            min_per_i = min_per_i)

    # simulate mortality
    simulate_mortality(fishpop_values = fishpop_values,
                       fishpop_track = as.matrix(fishpop_track[1:starting_values$pop_n, ]),
                       pop_n = starting_values$pop_n,
                       seafloor = seafloor$reef,
                       seafloor_values = seafloor_values,
                       parameters = parameters,
                       min_per_i = min_per_i)

    # diffuse values between neighbors
    simulate_diffusion(seafloor_values = seafloor_values,
                       cell_adj = cell_adj,
                       parameters = parameters)

    # update tracking list
    if (i %% save_each == 0) {

      if (verbose) {

        message("\r> ...Progress: ", round(i / max_i * 100), "% simulations runs... \t\t\t",
                appendLF = FALSE)

      }

      # get index where to store track data fish pop
      id_seafloor_start <- (i / save_each * nrow(seafloor_values)) + 1

      id_seafloor_end <- id_seafloor_start + nrow(seafloor_values) - 1

      # get index where to store track data fish pop
      id_fishpop_start <- (i / save_each * nrow(fishpop_values)) + 1

      id_fishpop_end <- id_fishpop_start + starting_values$pop_n - 1

      # save tracking data
      seafloor_track[id_seafloor_start:id_seafloor_end, ] <- seafloor_values

      fishpop_track[id_fishpop_start:id_fishpop_end, ] <- fishpop_values

    }
  }

  # new line after last progress message
  if (verbose) {

    message("")

    message("> ...Saving results...")

  }

  # add timestep counter
  seafloor_track$timestep <- rep(x = seq(from = 0, to = max_i, by = save_each),
                                 each = raster::ncell(seafloor))

  # add timestep counter
  if (starting_values$pop_n > 0) {

    fishpop_track$timestep <- rep(x = seq(from = 0, to = max_i, by = save_each),
                                  each = starting_values$pop_n)

  } else {

    fishpop_track$timestep <- numeric(0)

  }

  # combine result to list
  result <- list(seafloor = seafloor_track, fishpop = fishpop_track,
                 starting_values = starting_values, parameters = parameters,
                 max_i = max_i, min_per_i = min_per_i,
                 save_each = save_each, extent = extent, grain = raster::res(seafloor))

  # set class of result
  class(result) <- "mdl_rn"

  # new line after last progress message
  if (verbose) {

    message("")

    message("> All done.")

  }

  return(result)
}
