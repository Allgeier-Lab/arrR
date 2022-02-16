#' get_production
#'
#' @description
#' Summarize production of model run.
#'
#' @param result mdl_rn object.
#' @param lag Logical if TRUE, difference to previous time step is returned.
#'
#' @details
#' Function to summarize the bg and ag seagrass production of each time step.
#' The \code{lag} argument allows return the cumulative values (\code{FALSE}),
#' or the difference to the previous time step (\code{TRUE}).
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

  # sum for each time step
  seafloor_temp <- stats::aggregate(x = seafloor_temp[, -1],
                                    by = list(timestep = seafloor_temp$timestep),
                                    FUN = "sum")

  # use difference to previous time step
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
