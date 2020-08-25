#' update_i_pop
#'
#' @description Update the time step counter.
#'
#' @param population Data table with population data.
#' @param increase Numeric how much i is increased.
#'
#' @details
#' Internal function to increase the time step i after each simulated time step.
#'
#' @return data.table
#'
#' @aliases int_update_i_pop
#' @rdname int_update_i_pop
#'
#' @keywords internal
#'
#' @export
int_update_i_pop <- function(population, increase = 1){

  # data of current time step
  population_temp <- population[status == "living" & i == max(i)]

  # update/increase time step
  population_temp[, i := i + increase]

  # combine data frame with all data
  population <- rbind(population_temp, current)

  return(population)
}
