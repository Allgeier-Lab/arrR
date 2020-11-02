#' read_parameters
#'
#' @description Read parameters from text file
#'
#' @details
#' Construct a list with all default parameters. columns
#'
#' @param file String with path to text file.
#' @param return_list Logical if true parameters are returned as list.
#' @param ... Arguments passed on to \code{read.table}.
#'
#' @return list
#'
#' @aliases read_parameters
#' @rdname read_parameters
#'
#' @examples
#' # Add example code
#'
#' @export
read_parameters <- function(file, return_list = TRUE, ...) {

  # read parameters from file
  parameters <- utils::read.table(file, header = TRUE, ...)

  # check if cols have correct name
  if (!all(names(parameters) == c("parameter", "value"))) {

    stop("columns must be named 'parameter' and 'value'.",
         call. = FALSE)
  }

  # convert to list
  if (return_list) {

    # get names for list
    names_param <- parameters$parameter

    # convert values to list
    parameters <- as.list(parameters$value)

    # add names
    names(parameters) <- names_param

  }

  # return result
  return(parameters)
}
