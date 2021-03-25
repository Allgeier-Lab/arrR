#' summarize_mdlrn
#'
#' @description Summarize results.
#'
#' @param result mdl_rn object of simulation run.
#' @param summary String with summary functions. Must return one value when used
#' with aggregate().
#'
#' @details
#' Function to summarize results for each timestep.
#'
#' @return list
#'
#' @examples
#' # Add example code
#'
#' @aliases summarize_mdlrn
#' @rdname summarize_mdlrn
#'
#' @export
summarize_mdlrn <- function(result, summary = c("min", "mean", "max")) {

  # get timesteps
  timestep_seafloor <- result$seafloor$timestep

  # get cols to summarise
  seafloor <- subset(result$seafloor, select = c("ag_biomass", "bg_biomass",
                                                 "nutrients_pool", "detritus_pool",
                                                 "detritus_fish"))

  seafloor <- lapply(X = summary, function(i) {

    stats::aggregate(x = seafloor, by = list(timestep = timestep_seafloor),
                     FUN = i, na.rm = TRUE)

  })

  seafloor <- do.call(what = "rbind", args = seafloor)

  seafloor$summary <- rep(x = summary,
                          each = nrow(seafloor) / length(summary))

  # add burn_in col
  seafloor$burn_in <- ifelse(test = seafloor$timestep < result$burn_in,
                             yes = "yes", no = "no")

  if (nrow(result$fishpop > 0)) {

    # get timesteps
    timestep_fish <- result$fishpop$timestep

    # get cols to summarise
    fishpop <- subset(result$fishpop,
                      select = c("length", "weight",
                                 "died_consumption", "died_background"))

    fishpop <- lapply(X = summary, function(i) {

      stats::aggregate(x = fishpop, by = list(timestep = timestep_fish),
                       FUN = i, na.rm = TRUE)
    })

    fishpop <- do.call(what = "rbind", args = fishpop)

    fishpop$summary <- rep(x = summary,
                           each = nrow(fishpop) / length(summary))

    # add burn_in col
    fishpop$burn_in <- ifelse(test = fishpop$timestep < result$burn_in,
                              yes = "yes", no = "no")

  # no fish present
  } else {

    fishpop <- NA

  }

  result <- list(seafloor = seafloor, fishpop = fishpop)

  return(result)
}
