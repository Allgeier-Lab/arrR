#' summarize_results
#'
#' @description Summarize results.
#'
#' @param result mdl_rn object of simulation run.
#'
#' @details
#' Function to summarize results for each timestep.
#'
#' @return list
#'
#' @examples
#' # Add example code
#'
#' @aliases summarize_results
#' @rdname summarize_results
#'
#' @export
summarize_results <- function(result) {

  # get timesteps
  timestep_seafloor <- result$seafloor$timestep

  # get cols to summarise
  seafloor <- subset(result$seafloor, select = c("ag_biomass", "bg_biomass",
                                                 "nutrients_pool", "detritus_pool",
                                                 "detritus_dead"))

  # calc minimum
  seafloor_min <- stats::aggregate(x = seafloor, by = list(timestep = timestep_seafloor),
                                   FUN = "min", na.rm = TRUE)

  # calc mean
  seafloor_mean <- stats::aggregate(x = seafloor, by = list(timestep = timestep_seafloor),
                                    FUN = "mean", na.rm = TRUE)

  # calc max
  seafloor_max <- stats::aggregate(x = seafloor, by = list(timestep = timestep_seafloor),
                                   FUN = "max", na.rm = TRUE)

  # combine to one dataframe
  seafloor <- rbind(seafloor_min, seafloor_mean, seafloor_max)

  seafloor$summary <- rep(x = c("min", "mean", "max"),
                          each = nrow(seafloor) / 3)

  if (nrow(result$fishpop > 0)) {

    # get timesteps
    timestep_fish <- result$fishpop$timestep

    # get cols to summarise
    fishpop <- subset(result$fishpop,
                      select = c("length", "weight",
                                 "died_consumption", "died_background"))

    # calc summary fun
    fishpop_min <- stats::aggregate(x = fishpop,
                                    by = list(timestep = timestep_fish),
                                    FUN = "min", na.rm = TRUE)

    # calc summary fun
    fishpop_mean <- stats::aggregate(x = fishpop,
                                     by = list(timestep = timestep_fish),
                                     FUN = "mean", na.rm = TRUE)

    # calc summary fun
    fishpop_max <- stats::aggregate(x = fishpop,
                                    by = list(timestep = timestep_fish),
                                    FUN = "max", na.rm = TRUE)

    # combine to one dataframe
    fishpop <- rbind(fishpop_min, fishpop_mean, fishpop_max)

    fishpop$summary <- rep(x = c("min", "mean", "max"), each = nrow(fishpop) / 3)

  # no fish present
  } else {

    fishpop <- NA

  }

  result <- list(seafloor = seafloor, fishpop = fishpop)

  return(result)
}
