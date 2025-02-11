#' filter_mdlrn
#'
#' @description
#' Filter model run for specific time step.
#'
#' @param result mdl_rn object.
#' @param filter Integer with one or vector with min/max time step(s) to filter.
#' @param reset Logical if TRUE, cumulative seafloor values are reduced by value
#' before filter minimum.
#'
#' @details
#' This functions allows to return only specific time steps of a \code{mdl_rn} object
#' created with \code{\link{run_simulation}}.
#'
#' @return mdl_rn
#'
#' @examples
#' \dontrun{
#' filter_mdlrn(result = result_rand, filter = c(result_rand$max_i / 2, result_rand$max_i))
#' }
#'
#' @aliases filter_mdlrn
#' @rdname filter_mdlrn
#'
#' @export
filter_mdlrn <- function(result, filter = max(result$max_i), reset = FALSE) {

  # check if mdl_rn is provided
  if (!inherits(x = result, what = "mdl_rn")) {

    stop("Please provide 'mdl_rn' object created with 'run_simulation'.", call. = FALSE)

  }

  # repeat filter
  if (length(filter) == 1) {

    filter <- rep(x = filter, times = 2)

  } else if (length(filter) != 2) {

    stop("'filter' must be either one time step or min/max time steps.", call. = FALSE)

  }

  # check if all time steps are within boundaries
  if ((filter[1] < 0) || (filter[2] > result$max_i)) {

    stop("'filter' is not within 0 <= x <= max_i.", call. = FALSE)

  }

  # get seafloor value at last time step
  if (reset && filter[1] > 0) {

    # create vector with seafloor cols
    cols_seafloor <- c("x", "y", "ag_production", "bg_production", "ag_slough", "bg_slough",
                       "ag_uptake", "bg_uptake", "consumption", "excretion")

    # create vector with fishpop cols
    cols_fishpop <- c("id", "consumption", "excretion", "died_consumption", "died_background")

    # create vector with time steps i
    timestep_full <- seq(from = 0, to = result$max_i, by = result$save_each)

    # get last time step before filter
    timestep_last <- timestep_full[max(which(timestep_full < filter[1]))]

    # get values of last time step
    seafloor_last <- result$seafloor[result$seafloor$timestep == timestep_last,
                                     cols_seafloor]

    fishpop_last <- result$fishpop[result$fishpop$timestep == timestep_last,
                                   cols_fishpop]

    # get row ids where seafloor_last xy equals seafloor xy
    seafloor_rows <- rep(x = seq(from = 1, to = prod(result$dimensions)),
                         times = length(which(timestep_full >= filter[1] &
                                                timestep_full <= filter[2])))

    # get row ids where fishpop_last id equals fihspop id
    fishpop_rows <- rep(x = seq(from = 0, to = result$starting_values$pop_n)[-1],
                        times = length(which(timestep_full >= filter[1] &
                                               timestep_full <= filter[2])))

  }

  # get row id if time steps that are selected
  seafloor_id <- which(result$seafloor$timestep >= filter[1] &
                         result$seafloor$timestep <= filter[2])

  # get row id if time steps that are selected
  fishpop_id <-  which(result$fishpop$timestep >= filter[1] &
                         result$fishpop$timestep <= filter[2])

  # check if any iterations are left
  if (length(seafloor_id) == 0 && length(fishpop_id) == 0) {

    stop("No iterations left after applying 'filter'.", call. = FALSE)

  }

  # replace elements
  result$seafloor <- result$seafloor[seafloor_id, ]

  # replace elements
  result$fishpop <- result$fishpop[fishpop_id, ]

  # subtract all cumulative number until filter cutoff
  if (reset && filter[1] > 0) {

    # update cols seafloor
    result$seafloor[, cols_seafloor[-c(1, 2)]] <- result$seafloor[, cols_seafloor[-c(1, 2)]] -
      seafloor_last[seafloor_rows, cols_seafloor[-c(1, 2)]]

    # update cols fishpop
    result$fishpop[, cols_fishpop[-1]] <- result$fishpop[, cols_fishpop[-1]] -
      fishpop_last[fishpop_rows, cols_fishpop[-1]]

  }

  # replace elements
  result$max_i <- max(result$seafloor$timestep)

  return(result)
}
