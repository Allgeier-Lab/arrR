#' summarize_results
#'
#' @description Summarize results
#'
#' @param result mdl_rn object of simulation run.
#' @param summary_fun List with all model parameters.
#'
#' @details
#' Function to summarize results for each timestep.
#'
#'
#' @return data.frame
#'
#' @aliases summarize_results
#' @rdname summarize_results
#'
#' @export
summarize_results <- function(result, summary_fun) {

  # get timesteps
  timestep_seafloor <- result$seafloor$timestep

  # get cols to summarise
  seafloor <- subset(result$seafloor, select = c("ag_biomass", "bg_biomass",
                                                 "nutrients_pool", "detritus_pool",
                                                 "detritus_dead"))

  # calc summary fun
  seafloor <- stats::aggregate(x = seafloor, by = list(timestep = timestep_seafloor),
                               FUN = summary_fun, na.rm = TRUE)

  # get timesteps
  timestep_fish <- result$fish_population$timestep

  # get cols to summarise
  fish_population <- subset(result$fish_population,
                            select = c("length", "weight",
                                       "died_consumption", "died_background"))

  # calc summary fun
  fish_population <- stats::aggregate(x = fish_population,
                                      by = list(timestep = timestep_fish),
                                      FUN = summary_fun, na.rm = TRUE)

  result <- list(seafloor = seafloor, fish_population = fish_population)

  return(result)
}
