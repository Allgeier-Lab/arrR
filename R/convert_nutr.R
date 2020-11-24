#' convert_nutr
#'
#' @description Internal function to to convert nutrients
#'
#' @param x Vector with values to convert
#' @param to Character specifying unit to convert to. Either "g" (gram) or "um" (micromole).
#'
#' @details
#' Converts the amount of nutrients from gram to micromole or the other way around.
#' The conversion is based on the molecular mass of NH4.
#'
#' @return vector
#'
#' @aliases convert_nutr
#' @rdname convert_nutr
#'
#' @export
convert_nutr <- function(x, to) {

  # convert to gram by multiplying factor
  if (to == "g") {

   x * 18.039 / (10 ^ 6)

  # convert to umol by dividing factor
  } else if (to == "umol") {

   x * (10 ^ 6) / 18.039

  # throw error if bad option is set
  } else {

    stop("Please select either t='g' or to='umol'.",
         call. = FALSE)

  }
}
