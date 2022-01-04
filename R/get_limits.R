#' get_limits
#'
#' @description
#' Get list with min and max values for better plotting.
#'
#' @param result List with results of model run.
#'
#' @details
#' Returns list with minimum and maximum values of several model result runs for better
#' plotting. The list includes the range for i) ag_biommass, ii) bg_biomass, iii)
#' nutrients_pool, and iv) detritus_pool. Can be provided to the \code{limits} argument
#' of the \code{\link{plot.mdl_rn}} function.
#'
#' @return list
#'
#' @examples
#' \dontrun{
#' get_limits(result = list(result_rand, result_attr))
#' }
#'
#' @aliases get_limits
#' @rdname get_limits
#'
#' @export
get_limits <- function(result) {

  # check if mdl_rn is provided
  check_class <- vapply(X = result, FUN = function(x) inherits(x = x, what = "mdl_rn"),
                        FUN.VALUE = logical(1))

  if (!any(check_class)) {

    stop("Please provide 'mdl_rn' object createt with 'run_simulation()'.", call. = FALSE)

  }

  # loop through result list
  limits <- lapply(X = result, FUN = function(x) {

    # filter current result
    result_temp <- subset(x$seafloor, select = c("ag_biomass", "bg_biomass",
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

  # combine to matrix
  limits <- do.call(what = "rbind", args = limits)

  # create final list
  limits <- list(ag_biomass = c(min(limits[, 1]), max(limits[, 2])),
                 bg_biomass = c(min(limits[, 3]), max(limits[, 4])),
                 nutrients_pool = c(min(limits[, 5]), max(limits[, 6])),
                 detritus_pool = c(min(limits[, 7]), max(limits[, 8])))

  return(limits)

}
