#' update_i_envir
#'
#' @description Update the time step counter of the environment.
#'
#' @param environment Environment created with \code{\link{setup_environment}}.
#' @param environment_dt Data table of environment.
#' @param increase Numeric how much i is increased.
#'
#' @details
#' Internal function to increase the time step i after each simulated time step.
#'
#' @return data.table
#'
#' @aliases int_update_i_envir
#' @rdname int_update_i_envir
#'
#' @keywords internal
#'
#' @export
int_update_i_envir <- function(environment, environment_dt, increase = 1){

  # convert current environment to data table
  environment_temp <- int_as_data_table_ras(environment, xy = TRUE)

  # update/increase time step
  environment_temp$i <- max(environment_dt$i) + increase

  # combine data table with all data
  environment_dt <- rbind(environment_dt, environment_temp)

  return(environment_dt)
}
