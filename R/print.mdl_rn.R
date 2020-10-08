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
                            select = c(ag_biomass, bg_biomass, nutrients_pool,
                                       detritus_pool, detritus_dead))

  if (nrow(x$fish_population > 0)) {

    # get fish population values of last timestep
    fish_population_values <- subset(x$fish_population, timestep == max_i,
                                     select = c(length, weight,
                                                died_consumption, died_background))
  } else {

    fish_population_values <- rep(NA, times = 4)

  }

  # if return_mean = TRUE, no min or max can be calculated
  if (!is.null(x$use_summary)) {

    value_seafloor <- round(seafloor_values, digits = digits)

    value_population <- round(fish_population_values, digits = digits)

    summary_fun <- x$use_summary

    # print result
    cat(paste0("Total simulated time: ", max_i * x$min_per_i / 60 / 24, " days\n",
               "\n",
               "Seafloor: (ag_biomass, bg_biomass, nutrients_pool, detritus_pool, detritus_dead)\n",
               summary_fun, ": ", paste0(value_seafloor, collapse = ", "), "\n",
               "\n",
               "Fish population: (length, weight, died_consumption, died_background)\n",
               summary_fun, ": ", paste0(value_population, collapse = ", "), "\n",
               "\n"))

  # calculate min, mean, max
  } else {

  min_seafloor <- round(apply(X = seafloor_values, MARGIN = 2, FUN = min),
                        digits = digits)

  mean_seafloor <- round(apply(X = seafloor_values, MARGIN = 2, FUN = mean),
                         digits = digits)

  max_seafloor <- round(apply(X = seafloor_values, MARGIN = 2, FUN = max),
                        digits = digits)

  # no fish population present
  if (all(is.na(fish_population_values))) {

    min_fish_population <- NA

    mean_fish_population <- NA

    max_fish_population <- NA

  # fish population present
  } else {

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
             "Seafloor: (ag_biomass, bg_biomass, nutrients_pool, detritus_pool, detritus_dead)\n",
             "Minimum: ", paste0(min_seafloor, collapse = ", "), "\n",
             "Mean: ", paste0(mean_seafloor, collapse = ", "), "\n",
             "Maximum: ", paste0(max_seafloor, collapse = ", "), "\n",
             "\n",
             "Fish population: (length, weight, died_consumption, died_background)\n",
             "Minimum: ", paste0(min_fish_population, collapse = ", "), "\n",
             "Mean: ", paste0(mean_fish_population, collapse = ", "), "\n",
             "Maximum: ", paste0(max_fish_population, collapse = ", "), "\n"))

  }
}
