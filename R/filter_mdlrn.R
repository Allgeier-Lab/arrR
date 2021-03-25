#' filter_mdlrn
#'
#' @description Filter model run results
#'
#' @param result mdl_rn object of simulation run.
#' @param timestep Vector with timesteps to select
#'
#' @details
#' Function to filter results according to timestep
#'
#' @return data.frame
#'
#' @examples
#' # Add example code
#'
#' @aliases filter_mdlrn
#' @rdname filter_mdlrn
#'
#' @export
filter_mdlrn <- function(result, timestep = max(result$max_i)) {

  # check if mdl_rn is provided
  if (!inherits(x = result, what = "mdl_rn")) {

    stop("Please prove mdl_rn object createt with run_simulation.", call. = FALSE)

  }

  i <- timestep

  # check if i can be divided by save_each without reminder
  if (i %% result$save_each != 0) {

    stop("'timestep' was not saved during model run.",
         call. = FALSE)
  }

  # get row id if timesteps that are selected
  seafloor_id <- which(result$seafloor$timestep %in% i)

  # get row id if timesteps that are selected
  fishpop_id <- which(result$fishpop$timestep %in% i)

  # subset data.frame
  seafloor_track <- result$seafloor[seafloor_id, ]

  # subset data.frame
  fishpop_track <- result$fishpop[fishpop_id, ]

  # replace elements
  result$seafloor <- seafloor_track

  # replace elements
  result$fishpop <- fishpop_track

  # replace elements
  result$max_i <- max(i)

  return(result)
}
