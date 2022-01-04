#' get_production
#'
#' @description
#' Summarize production of model run.
#'
#' @param result mdl_rn object of simulation run.
#' @param lag Logical if cumulative or difference to previous timestep should be returned.
#'
#' @details
#' Function to summarize the ag and bg production of each timestep. The \code{lag}
#' argument allows to not return the cumulative value, but the difference to the previous
#' timestep.
#'
#' @return data.frame
#'
#' @examples
#' \dontrun{
#' get_production(result_rand)
#' }
#'
#' @aliases get_production
#' @rdname get_production
#'
#' @export
get_production <- function(result, lag = TRUE) {

  # select only required columns
  seafloor_temp <- result$seafloor[, c("timestep", "ag_production", "bg_production")]

  # sum for each timestep
  seafloor_temp <- stats::aggregate(x = seafloor_temp[, -1],
                                    by = list(timestep = seafloor_temp$timestep),
                                    FUN = "sum")

  # use difference to previous timestep
  if (lag) {

    # ag_production
    seafloor_temp[, 2] <- c(NA, seafloor_temp[2:nrow(seafloor_temp), 2] -
                              seafloor_temp[1:(nrow(seafloor_temp) - 1), 2])

    # bg_production
    seafloor_temp[, 3] <- c(NA, seafloor_temp[2:nrow(seafloor_temp), 3] -
                              seafloor_temp[1:(nrow(seafloor_temp) - 1), 3])

  }

  return(seafloor_temp)
}
