#' print.mdl_rn
#'
#' @description Printing method for mdl_rn object
#'
#' @param x Random patterns.
#' @param timestep Numeric with timestep to print.
#' @param digits Numeric of decimal places (round).
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
print.mdl_rn <- function(x, timestep = x$max_i, digits = 4, ...) {

  i <- timestep

  # check if i can be divided by save_each without reminder
  if (i %% x$save_each != 0) {

    stop("'timestep' was not saved during model run.",
         call. = FALSE)
  }

  # get seafloor values of selected timestep
  seafloor_values <- subset(x$seafloor, timestep == i,
                            select = c(ag_biomass, bg_biomass, nutrients_pool,
                                       detritus_pool, detritus_dead))

  min_seafloor <- round(apply(X = seafloor_values, MARGIN = 2,
                              FUN = min, na.rm = TRUE),
                        digits = digits)

  mean_seafloor <- round(apply(X = seafloor_values, MARGIN = 2,
                               FUN = mean, na.rm = TRUE),
                         digits = digits)

  max_seafloor <- round(apply(X = seafloor_values, MARGIN = 2,
                              FUN = max, na.rm = TRUE),
                        digits = digits)

  # no fish population present
  if (nrow(x$fish_population > 0)) {

    # get fish population values of selected timestep
    fish_population_values <- subset(x$fish_population, timestep == i,
                                     select = c(length, weight,
                                                died_consumption, died_background))

    # calculate min, median, max values
    min_fish_population <- round(apply(X = fish_population_values,
                                       MARGIN = 2, FUN = min, na.rm = TRUE),
                                 digits = digits)

    mean_fish_population <- round(apply(X = fish_population_values,
                                        MARGIN = 2, FUN = mean, na.rm = TRUE),
                                  digits = digits)

    max_fish_population <- round(apply(X = fish_population_values,
                                       MARGIN = 2, FUN = max, na.rm = TRUE),
                                 digits = digits)

  # fish population present
  } else {

    min_fish_population <- NA

    mean_fish_population <- NA

    max_fish_population <- NA

  }

  # print result
  cat(paste0("Total simulated time: ", max_i * x$min_per_i / 60 / 24, " days\n",
             "Saved each: ", save_each, " timesteps\n",
             "Results printed: ", i, " timestep\n",
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
