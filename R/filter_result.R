#' filter_result
#'
#' @description Filter result.
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
#' @aliases filter_result
#' @rdname filter_result
#'
#' @export
filter_result <- function(result, timestep = max(result$max_i)) {

  i <- timestep

  # check if i can be divided by save_each without reminder
  if (i %% result$save_each != 0) {

    stop("'timestep' was not saved during model run.",
         call. = FALSE)
  }

  # get row id if timesteps that are selected
  seafloor_id <- which(result$seafloor$timestep %in% i)

  # get row id if timesteps that are selected
  fish_population_id <- which(result$fish_population$timestep %in% i)

  # subset data.frame
  seafloor_track <- result$seafloor[seafloor_id, ]

  # subset data.frame
  fish_population_track <- result$fish_population[fish_population_id, ]

  # replace elements
  result$seafloor <- seafloor_track

  # replace elements
  result$fish_population <- fish_population_track

  # replace elements
  result$max_i <- max(i)

  return(result)
}
