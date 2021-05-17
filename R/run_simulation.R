#' run_simulation
#'
#' @description Core function to run model.
#'
#' @param seafloor RasterBrick with environment created with \code{\link{setup_seafloor}}.
#' @param fishpop Data.frame with fish population created with \code{\link{setup_fishpop}}.
#' @param parameters List with all model parameters.
#' @param nutr_input Vector with amount of nutrient input each timestep.
#' @param reef_attraction If TRUE, individuals are attracted to AR.
#' @param max_i Integer with maximum number of simulation timesteps.
#' @param min_per_i Integer to specify minutes per i.
#' @param seagrass_each Integer how often (each i * x) seagrass dynamics will be simulated.
#' @param save_each Numeric how often data should be saved to return.
#' @param burn_in Numeric with timesteps used to burn in.
#' @param return_burnin If FALSE all timesteps < burn_in are not returned.
#' @param extract Character to specify if only seafloor or fishpop should be returned as data.frame
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
                           parameters, nutr_input = NULL, reef_attraction,
                           max_i, min_per_i, seagrass_each = 1,
                           save_each = 1, burn_in = 0, return_burnin = TRUE,
                           extract = NULL, verbose = TRUE) {

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

  if (!is.null(nutr_input) && length(nutr_input) != max_i) {

    stop("'nutr_input' must have input amount for each iteration.", call. = FALSE)

  }

  if (burn_in >= max_i | burn_in < 0) {

    warning("'burn_in' larger than or equal to 'max_i' or 'burn_in' < 0.", call. = FALSE)

  }

  # check if fishpop is NULL
  if (is.null(fishpop)) {

    fishpop <- data.frame(id = numeric(), age = numeric(),
                          x = numeric(), y = numeric(), heading = numeric(),
                          length = numeric(), weight = numeric(),
                          reserves = numeric(), reserves_max = numeric(),
                          activity = numeric(), respiration = numeric(),
                          died_consumption = numeric(), died_background = numeric())

  # get 95% of movement distances
  } else {

    max_dist <- vapply(X = 1:1000000, FUN = function(i) {
      rcpp_rlognorm(mean = parameters$pop_mean_move,
                    sd = sqrt(parameters$pop_var_move),
                    min = 0, max = Inf)}, FUN.VALUE = numeric(1))

    max_dist <- stats::quantile(x = max_dist, probs = 0.95, names = FALSE)

  }

  # convert seafloor and fishpop as matrix
  seafloor_values <- as.matrix(raster::as.data.frame(seafloor, xy = TRUE))

  fishpop_values <- as.matrix(fishpop)

  # get mean starting values
  starting_values <- get_starting_values(seafloor_values = seafloor_values,
                                         fishpop_values = fishpop_values)

  # create lists to store results for each timestep
  seafloor_track <- vector(mode = "list", length = (max_i / save_each) + 1)

  fishpop_track <- vector(mode = "list", length = (max_i / save_each) + 1)

  # get extent of environment
  extent <- raster::extent(seafloor)

  # get dimensions of environment (nrow, ncol)
  dimensions <- dim(seafloor)[1:2]

  # get cell id of reef cells
  cells_reef <- which(seafloor_values[, 16] == 1)

  # get coordinates of reef cells
  coords_reef <- raster::xyFromCell(object = seafloor$reef,
                                    cell = cells_reef)

  # get neighboring cells for each focal cell using torus
  cell_adj <- get_neighbors(x = seafloor, direction = 8, torus = TRUE)

  # save input data in tracking data.frame
  seafloor_track[[1]] <- rlang::duplicate(seafloor_values)

  fishpop_track[[1]] <- rlang::duplicate(fishpop_values)

  # print some basic information about model run
  if (verbose) {

    message("> Seafloor with ", extent, "; ", nrow(coords_reef), " reef cells.")

    message("> Population with ", starting_values$pop_n, " individuals [reef_attraction: ", reef_attraction, "].")

    message("> Simulating ", max_i, " iterations [Burn-in: ", burn_in, " iter.].")

    message("> Saving each ", save_each, " iterations.")

    message("> One iteration equals ", min_per_i, " minutes.")

    message("")

    message("> ...Starting simulation...")

  }

  # simulate until max_i is reached
  for (i in 1:max_i) {

    # simulate nutrient input
    if (!is.null(nutr_input)) {

      simulate_input(seafloor_values = seafloor_values,
                     nutr_input = nutr_input,
                     timestep = i)

    }

    # simulate seagrass only each seagrass_each_i iteration
    if ((i * min_per_i) %% (min_per_i * seagrass_each) == 0) {

      # simulate seagrass growth
      simulate_seagrass(seafloor_values = seafloor_values,
                        parameters = parameters,
                        cells_reef = cells_reef,
                        time_frac = (min_per_i / 60) * seagrass_each)

      # redistribute detritus
      simulate_mineralization(seafloor_values = seafloor_values,
                              parameters = parameters)

    }

    if (i > burn_in & starting_values$pop_n != 0) {

      # simulate fish movement
      simulate_movement(fishpop_values = fishpop_values,
                        pop_n = starting_values$pop_n,
                        seafloor_values = seafloor_values,
                        parameters = parameters,
                        max_dist = max_dist,
                        reef_attraction = reef_attraction,
                        extent = extent,
                        dimensions = dimensions)

      # simulate fish respiration (26°C is mean water temperature in the Bahamas)
      simulate_respiration(fishpop_values = fishpop_values,
                           parameters = parameters,
                           water_temp = 26,
                           min_per_i = min_per_i)

      # simulate fishpop growth and including change of seafloor pools
      simulate_growth(fishpop_values = fishpop_values,
                      fishpop_track = fishpop_track[[1]],
                      pop_n = starting_values$pop_n,
                      seafloor = seafloor$reef,
                      seafloor_values = seafloor_values,
                      parameters = parameters,
                      min_per_i = min_per_i)

      # simulate mortality
      simulate_mortality(fishpop_values = fishpop_values,
                         fishpop_track = fishpop_track[[1]],
                         pop_n = starting_values$pop_n,
                         seafloor = seafloor$reef,
                         seafloor_values = seafloor_values,
                         parameters = parameters,
                         min_per_i = min_per_i)

    }

    # diffuse values between neighbors
    simulate_diffusion(seafloor_values = seafloor_values,
                       cell_adj = cell_adj,
                       parameters = parameters)

    # remove nutrients from cells
    simulate_output(seafloor_values = seafloor_values,
                    parameters = parameters)

    # update tracking list
    if (i %% save_each == 0) {

      seafloor_track[[i / save_each + 1]] <- rlang::duplicate(seafloor_values)

      fishpop_track[[i / save_each + 1]] <- rlang::duplicate(fishpop_values)

    }

    # print progress
    if (verbose) {

      message("\r> ...Progress: ", floor(i / max_i * 100), "% of total iterations... \t\t\t",
              appendLF = FALSE)

    }
  }

  # new line after last progress message
  if (verbose) {

    message("")

    message("> ...Saving results...")

  }

  # combine seafloor/fishpop to one dataframe
  seafloor_track <- data.frame(do.call(what = "rbind", args = seafloor_track))

  fishpop_track <- data.frame(do.call(what = "rbind", args = fishpop_track))

  # add timestep to  seafloor/fishpop counter
  seafloor_track$timestep <- rep(x = seq(from = 0, to = max_i, by = save_each),
                                 each = raster::ncell(seafloor))

  # fishpop is present
  if (starting_values$pop_n > 0) {

    fishpop_track$timestep <- rep(x = seq(from = 0, to = max_i, by = save_each),
                                  each = starting_values$pop_n)

  # no fish are present
  } else {

    fishpop_track$timestep <- numeric(0)

  }

  # add burn_in col
  seafloor_track$burn_in <- ifelse(test = seafloor_track$timestep < burn_in,
                                   yes = "yes", no = "no")

  fishpop_track$burn_in <- ifelse(test = fishpop_track$timestep < burn_in,
                                  yes = "yes", no = "no")

  # remove all burn_in values
  if (!return_burnin) {

    seafloor_track <- seafloor_track[seafloor_track$burn_in == "no", ]

    fishpop_track <- fishpop_track[fishpop_track$burn_in == "no", ]

  }

  # combine result to list
  result <- list(seafloor = seafloor_track, fishpop = fishpop_track,
                 starting_values = starting_values, parameters = parameters,
                 reef_attraction = reef_attraction,
                 nutr_input = ifelse(test = is.null(nutr_input), yes = NA, no = nutr_input),
                 max_i = max_i, min_per_i = min_per_i, burn_in = burn_in,
                 save_each = save_each, extent = extent, grain = raster::res(seafloor),
                 coords_reef = coords_reef)

  # set class of result
  class(result) <- "mdl_rn"

  # return only extract if != NULL
  if (!is.null(extract)) {

    if (verbose) {

      message("> ...Extract ", extract, " only...")

    }

    result <- extract_result(result = result, extract = extract)

  }

  # new line after last progress message
  if (verbose) {

    message("")

    message("> All done.")

  }

  return(result)
}
