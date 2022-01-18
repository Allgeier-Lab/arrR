#' print.mdl_rn
#'
#' @description
#' Printing method for \code{mdl_rn} object.
#'
#' @param x mdl_rn object.
#' @param digits Numeric of decimal places (round).
#' @param ... Arguments passed to cat.
#'
#' @details
#' Printing method for model run results simulated with \code{\link{run_simulation}}.
#' Includes some information about the model run itself and distribution values (min, mean, max)
#' for some values of the seafloor and fishpop.
#'
#' @examples
#' \dontrun{
#' print(result_rand)
#' }
#'
#'
#' @aliases print.mdl_rn
#' @rdname print.mdl_rn
#'
#' @export
print.mdl_rn <- function(x, digits = 3, ...) {

  # get seafloor values of selected timestep
  seafloor_values <- subset(x$seafloor, timestep == max(timestep),
                            select = c("ag_biomass", "bg_biomass", "nutrients_pool",
                                       "detritus_pool", "detritus_fish"))

  min_seafloor <- round(apply(X = seafloor_values, MARGIN = 2,
                              FUN = min, na.rm = TRUE),
                        digits = digits)

  mean_seafloor <- round(apply(X = seafloor_values, MARGIN = 2,
                               FUN = mean, na.rm = TRUE),
                         digits = digits)

  max_seafloor <- round(apply(X = seafloor_values, MARGIN = 2,
                              FUN = max, na.rm = TRUE),
                        digits = digits)

  #  fish population present
  if (nrow(x$fishpop > 0)) {

    # get fish population values of selected timestep
    fishpop_values <- subset(x$fishpop, timestep == max(timestep),
                             select = c("length", "weight",
                                        "died_consumption", "died_background"))

    # calculate min, median, max values
    min_fishpop <- round(apply(X = fishpop_values, MARGIN = 2, FUN = min, na.rm = TRUE),
                         digits = digits)

    mean_fishpop <- round(apply(X = fishpop_values, MARGIN = 2, FUN = mean, na.rm = TRUE),
                          digits = digits)

    max_fishpop <- round(apply(X = fishpop_values, MARGIN = 2, FUN = max, na.rm = TRUE),
                         digits = digits)

  # no fish population present
  } else {

    min_fishpop <- NA

    mean_fishpop <- NA

    max_fishpop <- NA

  }

  # get minimum timestep
  min_time <- unique(min(x$seafloor$timestep))

  # calculate total time in days rounded
  total_time <- round((x$max_i - min_time) * x$min_per_i / 60 / 24, digits = 2)

  # # calculate timestep in days rounded
  # total_time <- round(x$max_i * x$min_per_i / 60 / 24, digits = 2)

  save_time <- round(x$save_each * x$min_per_i / 60 / 24, digits = 2)

  # print result
  cat(paste0("Total time : ", paste0(c(min_time, x$max_i), collapse = "-"), " iterations (", total_time, " days) [Burn-in: ", x$burn_in, " iter.]\n",
             "Saved each : ", x$save_each, " iterations (", save_time, " days)\n",
             "Seafloor   : ", x$dimensions[1], " rows x " , x$dimensions[2],  " cols; ", nrow(x$coords_reef), " reef cell(s)\n",
             "Fishpop    : ", x$starting_values$pop_n, " indiv (movement: '", x$movement,"')\n",
             "\n",
             "Seafloor : (ag_biomass, bg_biomass, nutrients_pool, detritus_pool, detritus_fish)\n",
             "Minimum  : ", paste0(min_seafloor, collapse = ", "), "\n",
             "Mean     : ", paste0(mean_seafloor, collapse = ", "), "\n",
             "Maximum  : ", paste0(max_seafloor, collapse = ", "), "\n",
             "\n",
             "Fishpop  : (length, weight, died_consumption, died_background)\n",
             "Minimum  : ", paste0(min_fishpop, collapse = ", "), "\n",
             "Mean     : ", paste0(mean_fishpop, collapse = ", "), "\n",
             "Maximum  : ", paste0(max_fishpop, collapse = ", "), "\n"))

}
