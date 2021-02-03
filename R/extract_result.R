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

  # check if mdl_rn is provided
  if (!inherits(x = result, what = "mdl_rn")) {

    stop("Please prove mdl_rn object createt with run_simulation.", call. = FALSE)

  }

  # extract seafloor only
  if (extract == "seafloor") {

    result <- result$seafloor

  # extract fishpop only
  } else if (extract == "fishpop") {

    result <- result$fishpop

  # extract seafloor and fishpop
  } else if (extract == "both") {

    result <- list(seafloor = result$seafloor, fishpop = result$fishpop)

  # Throw error because wrong selection
  } else {

    stop("Please select either extract = 'seafloor', extract = 'fishpop', or extract = 'both'.",
         call. = FALSE)
  }

  return(result)
}
