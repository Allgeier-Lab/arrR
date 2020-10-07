#' print.mdl_rn
#'
#' @description Printing method for mdl_rn object
#'
#' @param x Random patterns.
#' @param digits Number of decimal places (round).
#' @param ... Arguments passed to cat
#'
#' @details
#' Printing method for model run results simulated with \code{\link{run_simulation}}.
#'
#' @seealso
#' \code{\link{run_simulation}}
#'
#' @examples
#' \dontrun{
#'
#' example code
#'
#' }
#'
#' @aliases print.mdl_rn
#' @rdname print.mdl_rn
#'
#' @export
print.mdl_rn <- function(x, digits = 4, ...) {

  # get max_i
  max_i <- x$max_i

  # get seafloor values of last timestep
  seafloor_values <- subset(x$seafloor, timestep == max_i,
                            select = c(ag_biomass, bg_biomass,
                                       detritus_pool, detritus_dead, nutrients_pool))

  # calculate min, median, max values
  min_seafloor <- round(apply(X = seafloor_values, MARGIN = 2, FUN = min),
                        digits = digits)

  mean_seafloor <- round(apply(X = seafloor_values, MARGIN = 2, FUN = mean),
                         digits = digits)

  max_seafloor <- round(apply(X = seafloor_values, MARGIN = 2, FUN = max),
                        digits = digits)

  # get fish population values of last timestep
  fish_population_values <- subset(x$fish_population, timestep == max_i,
                                   select = c(length, weight,
                                              died_consumption, died_background))

  # no fish population present
  if (nrow(fish_population_values) == 0) {

    min_fish_population <- NA

    mean_fish_population <- NA

    max_fish_population <- NA
  }

  # fish population present
  else {

    # calculate min, median, max values
    min_fish_population <- round(apply(X = fish_population_values,
                                       MARGIN = 2, FUN = min), digits = digits)

    mean_fish_population <- round(apply(X = fish_population_values,
                                        MARGIN = 2, FUN = mean), digits = digits)

    max_fish_population <- round(apply(X = fish_population_values,
                                       MARGIN = 2, FUN = max), digits = digits)

  }

  # print result
  cat(paste0("Total simulated time: ", max_i * x$min_per_i / 60 / 24, " days\n",
             "\n",
             "Seafloor: (ag_biomass, bg_biomass, detritus_pool, detritus_dead, nutrients_pool)\n",
             "Minimum: ", paste0(min_seafloor, collapse = ", "), "\n",
             "Mean: ", paste0(mean_seafloor, collapse = ", "), "\n",
             "Maximum: ", paste0(max_seafloor, collapse = ", "), "\n",
             "\n",
             "Fish population: (length, weight, died_consumption, died_background)\n",
             "Minimum: ", paste0(min_fish_population, collapse = ", "), "\n",
             "Mean: ", paste0(mean_fish_population, collapse = ", "), "\n",
             "Maximum: ", paste0(max_fish_population, collapse = ", "), "\n"))
}
