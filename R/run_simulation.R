#' run_simulation
#'
#' @description
#' Core function to run model.
#'
#' @param seafloor SpatRaster with environment created with \code{\link{setup_seafloor}}.
#' @param fishpop Data.frame with fish population created with \code{\link{setup_fishpop}}.
#' @param nutrients_input Vector with amount of nutrient input each timestep.
#' @param movement String specifing movement algorithm. Either 'rand', 'attr' or 'behav'.
#' @param parameters List with all model parameters.
#' @param max_i Integer with maximum number of simulation timesteps.
#' @param min_per_i Integer to specify minutes per i.
#' @param seagrass_each Integer how often (each i * x) seagrass dynamics will be simulated.
#' @param save_each Numeric how often data should be saved to return.
#' @param burn_in Numeric with timesteps used to burn in.
#' @param return_burnin If FALSE all timesteps < burn_in are not returned.
#' @param verbose If TRUE, progress reports are printed.
#'
#' @details
#' This is the core function of the \code{arrR} model that allows to easily run the
#' model. Besides running all sub-processes, the function also includes some basic
#' checks to make sure the model does not crash. However, this does not ensure that
#' e.g. all parameter values "make sense". The function returns a \code{mdl_rn} object
#' which stores besides the model run results a lot of information about the model run
#' specification and many function that can handle the objects exist (e.g. plotting).
#'
#' The functions is a 'wrapper' around the following sub-processes: (i) nutrient input,
#' (ii) seagrass growth, (iii) detritus mineralization, (iv) movement of individuals,
#' (v) respiration of individuals, (vi) growth of individuals, (vii) mortality of individuals,
#' (viii) diffusion of nutrients/detritus, and ix) nutrient output.
#'
#' The \code{movement} argument allows to either specify random movement of individuals,
#' attracted movement towards the artificial reef of individuals or a movement behavior based
#' on their biosenergetics.
#'
#' If \code{nutrients_input} is \code{0.0}, no nutrient input is simulated. To also simulate no
#' nutrient output, set the \code{nutrients_loss} parameter to zero.
#'
#' If \code{save_each > 1}, not all iterations are saved in the final \code{mdl_rn} object,
#' but only each timestep specified by the object. However, \code{max_i} must be dividable by
#' \code{save_each} without rest. Similar, \code{seagrass_each} allows to simulate all
#' seagrass sub-processes only each specified timestep.
#'
#' If \code{burn_in > 0}, all sub-processes related to fish individuals are not simulated
#' before this timestep is reached.
#'
#' @references
#' For a detailed model description see Esquivel, K.E., Hesselbarth, M.H.K., Allgeier, J.E.
#' Mechanistic support for increased primary production around artificial reefs. Manuscript
#' submitted for publication.
#'
#' @return mdl_rn
#'
#' @examples
#' \dontrun{
#' reef <- matrix(data = c(-1, 0, 0, 1, 1, 0, 0, -1, 0, 0),
#' ncol = 2, byrow = TRUE)
#'
#' seafloor <- setup_seafloor(dimensions = c(100, 100), grain = 1,
#' reef = reef, starting_values = arrR_starting_values)
#' fishpop <- setup_fishpop(seafloor = seafloor,
#' starting_values = arrR_starting_values, parameters = arrR_parameters)
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
                           max_i, min_per_i,seagrass_each = 1,
                           save_each = 1, burn_in = 0, return_burnin = TRUE,
                           verbose = TRUE) {

  # get time at beginning for final print
  if (verbose) {

    t_start <- Sys.time()

  }

  # check input and warnings #

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

  # check if each i has input
  if ((length(nutrients_input) != (max_i / seagrass_each)) && (length(nutrients_input) != 1)) {

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

  # convert seafloor and fishpop as matrix
  seafloor_values <- as.matrix(terra::as.data.frame(seafloor, xy = TRUE, na.rm = FALSE))

  # get extent of environment
  extent <- as.vector(terra::ext(seafloor))

  # get dimensions of environment (nrow, ncol)
  dimensions <- dim(seafloor)[1:2]

  # create lists to store results for each timestep
  seafloor_track <- vector(mode = "list", length = (max_i / save_each) + 1)

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

      warning("No 'fishpop' present. Setting movement argument to 'rand'",
              call. = FALSE)
    }

  # set behavior to foraging only if movement != "behav"
  } else {

    if (movement %in% c("rand", "attr")) {

      fishpop$behavior <- 3.0

    }
  }

  # check if no reef is present but movement not rand
  if ((sum(seafloor_values[, "reef"]) == 0) && (movement %in% c("attr", "behav"))) {

    movement <- "rand"

    if (verbose) {

      warning("No reef cell(s) present. Thus 'movement' set to 'rand'.", call. = FALSE)

    }
  }

  fishpop_values <- as.matrix(fishpop)

  fishpop_track <- vector(mode = "list", length = (max_i / save_each) + 1)

  # setup various #

  # get mean starting values
  starting_values <- .get_starting_values(seafloor_values = seafloor_values,
                                          fishpop_values = fishpop_values)

  # print model run characteristics #

  total_time <- round(max_i * min_per_i / 60 / 24, digits = 2)

  total_save_each <- round(save_each * min_per_i / 60 / 24, digits = 2)

  # print some basic information about model run
  if (verbose) {

    message("> ...Starting at <", t_start, ">...")

    message("")

    message("> Seafloor with ", dimensions[1], " rows x ", dimensions[2], " cols; ", sum(seafloor_values[, "reef"] == 1), " reef cell(s).")

    message("> Population with ", starting_values$pop_n, " individuals [movement: '", movement, "'].")

    message("> Simulating ", max_i, " iterations (", total_time, " days) [Burn-in: ", burn_in, " iter.].")

    message("> Saving each ", save_each, " iterations (", total_save_each, " days).")

    message("> One iteration equals ", min_per_i, " minutes.")

    message("")

  }

  .rcpp_simulate(seafloor = seafloor_values, fishpop = fishpop_values, nutrients_input = nutrients_input,
                 seafloor_track = seafloor_track, fishpop_track = fishpop_track,
                 parameters = parameters, movement = movement,
                 extent = extent, dimensions = dimensions, max_i = max_i, min_per_i = min_per_i,
                 save_each = save_each, seagrass_each = seagrass_each, burn_in = burn_in,
                 verbose = verbose)

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
                                 each = terra::ncell(seafloor))

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
  result <- list(seafloor = seafloor_track, fishpop = fishpop_track, nutrients_input = nutrients_input,
                 movement = movement, parameters = parameters, starting_values = starting_values,
                 extent = extent, grain = terra::res(seafloor), dimensions = dimensions,
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
