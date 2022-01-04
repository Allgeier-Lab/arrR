#' summarize_mdlrn
#'
#' @description
#' Summarize results of model run.
#'
#' @param result mdl_rn object of simulation run.
#' @param what Vector with 'seafloor' and/or 'fishpop' to specifiy what to summarize.
#' @param summary String with summary functions. Must return one value when used
#' with aggregate().
#' @param verbose Logical if warning messages should be printed.
#'
#' @details
#' Function to summarize results for each timestep. The \code{summary} argument
#' allows to specify which summary statistics are used for each cell. The selected
#' statistics are used by \code{\link{aggregate}} and must return one value.
#'
#' For the seafloor i) ag_biomass, ii) bg_biomass, iii) nutrients_pool, iv) detritus_pool
#' are returned. For the fish population i) length, ii) weight, iii) died_consumption, and
#' iv) died_background are returned.
#'
#' @return list
#'
#' @examples
#' \dontrun{
#' summarize_mdlrn(result_rand)
#' }
#'
#' @aliases summarize_mdlrn
#' @rdname summarize_mdlrn
#'
#' @export
summarize_mdlrn <- function(result, what = c("seafloor", "fishpop"),
                            summary = c("min", "mean", "max"), verbose = TRUE) {

  # check if what arguments makes sense
  if (!all(what %in% c("seafloor", "fishpop"))) {

    stop("'what' must be either 'seafloor' and/or 'fishpop'.", call. = FALSE)

  }

  # check if there is fishpop
  if (nrow(result$fishpop) == 0 && "fishpop" %in% what) {

    # print warning
    if (verbose) {

      warning("No fish population present. Only summarizing seafloor.", call. = FALSE)

    }

    # only return seafloor
    what <- "seafloor"

  }

  result_sum <- lapply(X = seq_along(what), function(i) {

    if (what[i] == "seafloor") {

      # create vector with columns
      cols_temp <- c("ag_biomass", "bg_biomass", "nutrients_pool", "detritus_pool")

      # get timesteps
      timestep_temp <- result$seafloor$timestep

    } else {

      # create vector with columns
      cols_temp <- c("length", "weight", "died_consumption", "died_background")

      # get timesteps
      timestep_temp <- result$fishpop$timestep

    }

    # subset data
    data_temp <- subset(x = result[[what[i]]], select = cols_temp)

    # calc summary and combine to df
    data_temp <- do.call(what = "rbind", args = lapply(X = summary, function(j) {

      cbind(stats::aggregate(x = data_temp, by = list(timestep = timestep_temp),
                             FUN = j, na.rm = TRUE), summary = j)}))

    # add burn_in col
    data_temp$burn_in <- ifelse(test = data_temp$timestep < result$burn_in,
                                yes = "yes", no = "no")

    return(data_temp)

  })

  # set names of list
  names(result_sum) <- what

  return(result_sum)
}
