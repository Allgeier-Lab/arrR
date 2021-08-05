#' filter_mdlrn
#'
#' @description
#' Filter model run for specific timestep.
#'
#' @param result mdl_rn object of simulation run.
#' @param timestep Vector with timesteps to select
#'
#' @details
#' This functions allows to return only specific timesteps of a \code{mdl_rn} object
#' created with \code{\link{run_simulation}}. The function ensures that the object will
#' still be a \code{mdl_rn} object. Currently, only one timestep can be returned.
#'
#' @return mdl_rn
#'
#' @examples
#'  \dontrun{
#' filter_mdlrn(result = result_rand, timestep = 10200)
#' }
#'
#' @aliases filter_mdlrn
#' @rdname filter_mdlrn
#'
#' @export
filter_mdlrn <- function(result, timestep = max(result$max_i)) {

  # check if mdl_rn is provided
  if (!inherits(x = result, what = "mdl_rn")) {

    stop("Please provide 'mdl_rn' object created with 'run_simulation'.", call. = FALSE)

  }

  # get timestep
  timestep_slctd <- timestep

  # check if timestep_slctd can be divided by save_each without reminder
  if (timestep_slctd %% result$save_each != 0) {

    stop("'timestep' was not saved during model run.",
         call. = FALSE)
  }

  # get row id if timesteps that are selected
  seafloor_id <- which(result$seafloor$timestep %in% timestep_slctd)

  # get row id if timesteps that are selected
  fishpop_id <- which(result$fishpop$timestep %in% timestep_slctd)

  # subset data.frame
  seafloor_track <- result$seafloor[seafloor_id, ]

  # subset data.frame
  fishpop_track <- result$fishpop[fishpop_id, ]

  # replace elements
  result$seafloor <- seafloor_track

  # replace elements
  result$fishpop <- fishpop_track

  # replace elements
  result$max_i <- max(timestep_slctd)

  return(result)
}
