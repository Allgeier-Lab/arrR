#' extract_result
#'
#' @description Extract result
#'
#' @param result List with results of model run.
#' @param extract Character specifying if "seafloor" or "fishpop" is extracted.
#'
#' @details
#' Extract only seafloor or fishpop values as data.frame.
#'
#' @return data.frame
#'
#' @examples
#' # Add example code
#'
#' @aliases extract_result
#' @rdname extract_result
#'
#' @export
extract_result <- function(result, extract) {

  # extract seafloor only
  if (extract == "seafloor") {

    result <- result$seafloor

  # extract fishpop only
  } else if (extract == "fishpop") {

    result <- result$fishpop

  # Throw error because wrong selection
  } else {

    stop("Please select either extract = 'seafloor' or extract = 'fishpop'.",
         call. = FALSE)
  }

  return(result)
}
