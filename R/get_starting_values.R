#' get_starting_values
#'
#' @description
#' Get mean starting values of model run.
#'
#' @param seafloor_values Matrix with seafloor values.
#' @param fishpop_values Matrix population created with \code{\link{setup_fishpop}}.
#'
#' @details
#' Internal function to get mean starting values during \code{\link{run_simulation}}.
#' Includes values for i) bg_biomass, ii) ag_biomass, iii) nutrients_pool, iv) detritus_pool,
#' v) pop_n, and vi) pop_mean_size. The function is needed because some of the values
#' can include stochasticity  and thus differ from the starting values.
#'
#' @return list
#'
#' @examples
#' \dontrun{
#' get_starting_values(seafloor_values = seafloor_values, fishpop_values = fishpop_values)
#' }
#'
#' @aliases get_starting_values
#' @rdname get_starting_values
#'
#' @keywords internal
get_starting_values <- function(seafloor_values, fishpop_values) {

  # get mean values and # fish pop
  bg_biomass <- mean(seafloor_values[, "bg_biomass"], na.rm = TRUE)

  ag_biomass <- mean(seafloor_values[, "ag_biomass"], na.rm = TRUE)

  nutrients_pool <- mean(seafloor_values[, "nutrients_pool"], na.rm = TRUE)

  detritus_pool <- mean(seafloor_values[, "detritus_pool"], na.rm = TRUE)

  pop_n <- nrow(fishpop_values)

  pop_mean_size <- mean(fishpop_values[, "length"])

  # combine to result list
  result <- list(bg_biomass = bg_biomass, ag_biomass = ag_biomass,
                 nutrients_pool = nutrients_pool, detritus_pool = detritus_pool,
                 pop_n = pop_n, pop_mean_size = pop_mean_size)

  return(result)
}
