#' summarize_results
#'
#' @description Summarize results
#'
#' @param result mdl_rn object of simulation run.
#'
#' @details
#' Function to summarize results for each timestep.
#'
#'
#' @return data.frame
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
                          each = (result$max_i / result$save_each) + 1)

  if (nrow(result$fish_population > 0)) {

    # get timesteps
    timestep_fish <- result$fish_population$timestep

    # get cols to summarise
    fish_population <- subset(result$fish_population,
                              select = c("length", "weight",
                                         "died_consumption", "died_background"))

    # calc summary fun
    fish_population_min <- stats::aggregate(x = fish_population,
                                            by = list(timestep = timestep_fish),
                                            FUN = "min", na.rm = TRUE)

    # calc summary fun
    fish_population_mean <- stats::aggregate(x = fish_population,
                                             by = list(timestep = timestep_fish),
                                             FUN = "mean", na.rm = TRUE)

    # calc summary fun
    fish_population_max <- stats::aggregate(x = fish_population,
                                            by = list(timestep = timestep_fish),
                                            FUN = "max", na.rm = TRUE)

    # combine to one dataframe
    fish_population <- rbind(fish_population_min, fish_population_mean, fish_population_max)

    fish_population$summary <- rep(x = c("min", "mean", "max"),
                                   each = (result$max_i / result$save_each) + 1)

  # no fish present
  } else {

    fish_population <- NA

  }

  result <- list(seafloor = seafloor, fish_population = fish_population)

  return(result)
}
