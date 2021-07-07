#' read_parameters
#'
#' @description
#' Read parameters from text file.
#'
#' @details
#' Creates a list with all parameters or starting values. Columns must be named 'parameter' and
#' 'value'.
#'
#' @param file String with path to text file.
#' @param sep String with separator of columns.
#' @param return_list Logical if TRUE parameters are returned as list.
#' @param ... Arguments passed on to \code{read.table}.
#'
#' @return list
#'
#' @aliases read_parameters
#' @rdname read_parameters
#'
#' @examples
#' \dontrun{
#' read_parameters(file = "starting-values.csv", sep = ";")
#' }
#'
#' @export
read_parameters <- function(file, sep =";", return_list = TRUE, ...) {

  # read parameters from file
  parameters <- utils::read.table(file, sep = sep, header = TRUE, ...)

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

    # check_parameters(parameters)

  }

  # return result
  return(parameters)
}
