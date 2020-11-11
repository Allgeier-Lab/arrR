#' get_limits
#'
#' @description Get list with min and max values of seafloor for better plotting
#'
#' @param result List with results of model run.
#' @param timestep Integer to specify which timestep is plotted.
#'
#' @details
#' Returns list with minimum and maximum values of several result runs for better
#' plotting. Can be provided to the \code{limits} argument of the
#' \code{\link{plot.mdl_rn}} function.
#'
#' @return list
#'
#' @examples
#' # Add example code
#'
#' @aliases get_limits
#' @rdname get_limits
#'
#' @export
get_limits <- function(result, timestep = result[[1]]$max_i) {

  # get timestep
  i <- timestep

  # loop through result list
  limits <- lapply(X = result, FUN = function(x) {

    # filter current result
    result_temp <- subset(x$seafloor, timestep == i,
                          select = c("ag_biomass", "bg_biomass",
                                     "nutrients_pool", "detritus_pool"))

    # get min and max values
    c(ag_min = min(result_temp$ag_biomass, na.rm = TRUE),
      ag_max = max(result_temp$ag_biomass, na.rm = TRUE),

      bg_min = min(result_temp$bg_biomass, na.rm = TRUE),
      bg_max = max(result_temp$bg_biomass, na.rm = TRUE),

      nutr_min = min(result_temp$nutrients_pool, na.rm = TRUE),
      nutr_max = max(result_temp$nutrients_pool, na.rm = TRUE),

      detr_min = min(result_temp$detritus_pool, na.rm = TRUE),
      detr_max = max(result_temp$detritus_pool, na.rm = TRUE)
    )

  })

  # combind to matrix
  limits <- do.call(what = "rbind", args = limits)

  # create final list
  limits <- list(ag_biomass = c(min(limits[, 1]), max(limits[, 2])),
                 bg_biomass = c(min(limits[, 3]), max(limits[, 4])),
                 nutrients_pool = c(min(limits[, 5]), max(limits[, 6])),
                 detritus_pool = c(min(limits[, 7]), max(limits[, 8])))

  return(limits)

}

