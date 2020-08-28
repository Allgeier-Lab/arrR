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
    data_current <- raster::as.data.frame(data_current, xy = TRUE)
  }

  # check if data is available
  if (nrow(data_current > 0)) {

    # update/increase time step
    data_current$track_i <- max(data_track$track_i) + increase

  }

  # combine data table with all data
  rbind(data_track, data_current)
}
