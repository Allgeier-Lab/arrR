#' update_i_pop
#'
#' @description Update the time step counter.
#'
#' @param data_current Data table with current population data.
#' @param data_track Data table with history of population data.
#' @param ras If TRUE data_current will be converted from raster to data.table.
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
int_update_i <- function(data_current, data_track, ras = FALSE, increase = 1){

  if (ras) {
    data_current <- int_as_data_table_ras(data_current, xy = TRUE)
  }

  # update/increase time step
  data_current[, i := max(data_track$i) + increase]

  # combine data table with all data
  data_track <- rbind(data_track, data_current)

  return(data_track)
}

#### old function ####

# # data of current time step
# population_temp <- population[status == "living" & i == max(i)]
#
# # update/increase time step
# population_temp[, i := i + increase]
#
# # combine data frame with all data
# population <- rbind(population_temp, current)
#
# return(population)


# # convert current environment to data table
# environment_current <- int_as_data_table_ras(environment_current, xy = TRUE)
#
# # update/increase time step
# environment_current$i <- max(environment_track$i) + increase
#
# # combine data table with all data
# environment_track <- rbind(environment_track, environment_current)
#
# return(environment_track)
