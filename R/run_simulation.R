#' run_simulation
#'
#' @description
#' Run simulation.
#'
#' @param seafloor Data.frame with seafloor.
#' @param fishpop Data.frame with fish population.
#' @param nutrients_input Vector with nutrient input for each time step.
#' @param movement String specifying movement algorithm.
#' @param parameters List with all model parameters.
#' @param max_i Integer with maximum number of simulation time steps.
#' @param min_per_i Integer to specify minutes per i.
#' @param seagrass_each Integer how often (each i * x) seagrass dynamics will be simulated.
#' @param save_each Numeric how often data should be saved.
#' @param burn_in Numeric with time steps used to burn in (no fish).
#' @param return_burnin If FALSE, all time steps < burn_in are not saved.
#' @param to_disk Logical if TRUE, results are written into a text file.
#' @param path_disk String with path to result text file.
#' @param verbose Logical if TRUE, progress reports are printed.
#'
#' @details
#' This is the main function of the simulation model. Besides running all sub-processes,
#' the function also includes some basic checks to make sure the model does not crash.
#' However, this does not ensure that all parameter values "make sense". The function
#' returns a \code{mdl_rn} object, which stores the model run results and additionally
#' information about the model run itself.
#'
#' The functions is a 'wrapper' around the following sub-processes: (i) nutrient input,
#' (ii) seagrass growth, (iii) detritus mineralization, (iv) movement of individuals,
#' (v) respiration of individuals, (vi) growth of individuals, (vii) mortality of individuals,
#' (viii) diffusion of nutrients/detritus, and ix) nutrient output.
#'
#' If \code{nutrients_input} is zero (default), no nutrient input is simulated.
#' To simulate no nutrient output, set the \code{nutrients_loss} parameter to zero.
#'
#' The \code{movement} argument allows to either specify random movement of individuals
#' (\code{'rand'}), attracted movement towards the artificial reef cells of individuals
#' (\code{'attr'}) or a movement behavior based on their biosenergetics (\code{'behav'}).
#'
#' \code{seagrass_each} allows to simulate all seagrass sub-processes only each
#' specified time step.
#'
#' If \code{save_each > 1}, not all iterations are saved in the final \code{mdl_rn} object,
#' but only each time step specified by the argument. \code{max_i} must be dividable by
#' \code{save_each} without rest.
#'
#' If \code{burn_in > 0}, all sub-processes related to fish individuals are not simulated
#' before this time step is reached.
#'
#' @references
#' For a detailed model description, please see Esquivel, K.E., Hesselbarth, M.H.K.,
#' Allgeier, J.E., 2022. Mechanistic support for increased primary production around
#' artificial reefs. Ecological Applications e2617. <https://doi.org/10.1002/eap.2617>
#'
#' @return mdl_rn
#'
#' @examples
#' \dontrun{
#' reef <- matrix(data = c(-1, 0, 0, 1, 1, 0, 0, -1, 0, 0),
#' ncol = 2, byrow = TRUE)
#'
#' seafloor <- setup_seafloor(dimensions = c(50, 50), grain = 1,
#' reef = reef, starting_values = default_starting)
#' fishpop <- setup_fishpop(seafloor = seafloor,
#' starting_values = default_starting, parameters = default_parameters)
#'
#' run_simulation(seafloor = seafloor, fishpop = fishpop, parameters = parameters,
#' max_i = 1000, min_per_i = 120, save_each = 10)
#' }
#'
#' @aliases run_simulation
#' @rdname run_simulation
#'
#' @export
run_simulation <- function(seafloor, fishpop, nutrients_input = 0.0,
                           movement = "rand", parameters,
                           max_i, min_per_i, seagrass_each = 1,
                           save_each = 1, burn_in = 0, return_burnin = TRUE,
                           to_disk = FALSE, path_disk = NULL, verbose = TRUE) {

  # get time at beginning for final print
  if (verbose) {

    t_start <- Sys.time()

  }

  # check input and warnings #
  if (any(names(fishpop) != c("id", "age", "x", "y", "heading", "length", "weight",
                              "activity", "respiration", "reserves", "reserves_max",
                              "behavior", "consumption", "excretion", "died_consumption",
                              "died_background"))) {

      stop("Please provide fish population created with 'setup_fishpop()'", call. = FALSE)

  }

  # check input and warnings #
  if (any(names(seafloor) != c("x", "y", "ag_biomass", "bg_biomass", "nutrients_pool",
                               "detritus_pool", "detritus_fish", "ag_production", "bg_production",
                               "ag_slough", "bg_slough", "ag_uptake", "bg_uptake", "consumption",
                               "excretion", "reef" ))) {

    stop("Please provide seafloor created with 'setup_seafloor()'", call. = FALSE)

  }

  # check parameters
  param_warnings <- tryCatch(check_parameters(parameters = parameters, verbose = FALSE),
                             warning = function(wrn) wrn)

  # stop with error
  if (length(param_warnings$message) > 0) {

    stop(param_warnings$message, call. = FALSE)

  }

  if (any(fishpop$length > parameters$pop_ldie)) {

    warning("Some individuals are larger than pop_ldie.", call. = FALSE)

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

  # check if each i has input
  if ((length(nutrients_input) != max_i) && (length(nutrients_input) != 1)) {

    stop("'nutrients_input' must have input amount for each iteration.", call. = FALSE)

  }

  # check if move is valid
  if (!movement %in% c("rand", "attr", "behav")) {

    stop("Please select either 'rand', 'attr' or 'behav' as movement type.",
         call. = FALSE)

  }

  # check if burn in makes sense
  if (burn_in >= max_i || burn_in < 0) {

    # set to 0
    burn_in <- 0

    if (verbose) {

      warning("'burn_in' larger than or equal to 'max_i' or 'burn_in' < 0. Setting to burn_in = 0", call. = FALSE)

    }
  }

  # setup seafloor #

  # get seafloor dimensions, extent, grain
  seafloor_dim <- get_seafloor_dim(seafloor = seafloor)

  dimensions <- seafloor_dim$dimensions

  extent <-  seafloor_dim$extent

  grain <- seafloor_dim$grain

  # convert seafloor and fishpop as matrix
  seafloor_values <- as.matrix(seafloor)

  # create lists to store results for each time step
  # even if to_disk = T, starting matrix is returned
  seafloor_track <- vector(mode = "list", length = ifelse(test = to_disk, yes = 1, no = (max_i / save_each) + 1))

  # setup fishpop #

  # check if fishpop is NULL
  if (is.null(fishpop) || nrow(fishpop) == 0) {

    if (is.null(fishpop)) {

      # create empty dataframe
      fishpop <- setup_fishpop(seafloor = seafloor, starting_values = list(pop_n = 0),
                               verbose = FALSE)

    }

    # set movement to rand because no fish
    if (movement != "rand") {

      movement <- "rand"

      if (verbose) {

        warning("No 'fishpop' present. Setting movement argument to 'rand'",
                call. = FALSE)

      }
    }
  }

  # check if no reef is present but movement not rand
  if ((sum(seafloor_values[, "reef"]) == 0) && (movement %in% c("attr", "behav"))) {

    movement <- "rand"

    if (verbose) {

      warning("No reef cell(s) present. Thus 'movement' set to 'rand'.", call. = FALSE)

    }
  }

  # convert seafloor and fishpop as matrix
  fishpop_values <- as.matrix(fishpop)

  # create lists to store results for each time step
  # even if to_disk = T, timestep=0 is needed during simulation
  fishpop_track <- vector(mode = "list", length = ifelse(test = to_disk, yes = 1, no = (max_i / save_each) + 1))

  # setup various #

  # get mean starting values
  starting_values <- get_starting_values(seafloor_values = seafloor_values,
                                         fishpop_values = fishpop_values)

  # print model run characteristics #

  total_time <- round(max_i * min_per_i / 60 / 24, digits = 2)

  total_save_each <- round(save_each * min_per_i / 60 / 24, digits = 2)

  # get disk to result text file if not specified
  path_disk <- ifelse(test = is.null(path_disk), yes = getwd(), no = path_disk)

  # print some basic information about model run
  if (verbose) {

    message("> ...Starting at <", t_start, ">...")

    message("")

    message("> Seafloor with ", dimensions[1], " rows x ", dimensions[2], " cols; ", sum(seafloor_values[, "reef"] == 1), " reef cell(s).")

    message("> Population with ", starting_values$pop_n, " individuals [movement: '", movement, "'].")

    message("> Simulating ", max_i, " iterations (", total_time, " days) [Burn-in: ", burn_in, " iter.].")

    message("> Saving each ", save_each, " iterations (", total_save_each, " days).")

    message("> One iteration equals ", min_per_i, " minutes.")

    if (to_disk) {

      message("> Writing results to ", path_disk, ".")

    } else {

      message("> Storing results in RAM.")

    }

    message("")

  }

  rcpp_simulate(seafloor = seafloor_values, fishpop = fishpop_values, nutrients_input = nutrients_input,
                seafloor_track = seafloor_track, fishpop_track = fishpop_track,
                parameters = parameters, movement = movement,
                extent = extent, dimensions = dimensions, max_i = max_i, min_per_i = min_per_i,
                save_each = save_each, seagrass_each = seagrass_each, burn_in = burn_in,
                to_disk = to_disk, path_disk = path_disk, verbose = verbose)

   # new line after last progress message
  if (verbose) {

    message("")

    message("> ...Saving results...")

  }

  # combine seafloor/fishpop to one dataframe
  seafloor_track <- data.frame(do.call(what = "rbind", args = seafloor_track))

  fishpop_track <- data.frame(do.call(what = "rbind", args = fishpop_track))

  # create timestep vector to add to data.frames
  # results written to disk, only initial data.frame present
  if (to_disk) {

    # only initial timestep present
    timestep_seafloor_temp <- 0

    # either only initial timestep present or no fishpop
    timestep_fishpop_temp <- ifelse(test = starting_values$pop_n > 0,
                                    yes = 1, no = 0)

    timestep_fishpop_temp <- numeric(timestep_fishpop_temp)

  # data.frame stored in RAM
  } else {

    # repeat all saved timesteps as often as cells are present
    timestep_seafloor_temp <- rep(x = seq(from = 0, to = max_i, by = save_each),
                                  each = nrow(seafloor))

    # repeat all saved timsteps as often as individuals are present
    timestep_fishpop_temp <- rep(x = seq(from = 0, to = max_i, by = save_each),
                                 each = starting_values$pop_n)

  }

  # add timesteps as column
  seafloor_track$timestep <- timestep_seafloor_temp

  fishpop_track$timestep <- timestep_fishpop_temp

  # add burn_in col
  seafloor_track$burn_in <- ifelse(test = seafloor_track$timestep > burn_in,
                                   yes = "no", no = "yes")

  fishpop_track$burn_in <- ifelse(test = fishpop_track$timestep > burn_in,
                                  yes = "no", no = "yes")

  # remove all burn_in values
  if (!return_burnin) {

    seafloor_track <- seafloor_track[seafloor_track$burn_in == "no", ]

    fishpop_track <- fishpop_track[fishpop_track$burn_in == "no", ]

  }

  # combine result to list
  result <- list(seafloor = seafloor_track, fishpop = fishpop_track, nutrients_input = nutrients_input,
                 movement = movement, parameters = parameters, starting_values = starting_values,
                 dimensions = dimensions, extent = extent, grain = grain,
                 max_i = max_i, min_per_i = min_per_i, burn_in = burn_in,
                 seagrass_each = seagrass_each, save_each = save_each)

  # set class of result
  class(result) <- "mdl_rn"

  # new line after last progress message
  if (verbose) {

    # get time at end
    t_end <- Sys.time()

    message("")

    message("> ...All done at <", t_end, ">...")

  }

  return(result)
}
