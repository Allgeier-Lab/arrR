#' convert_n
#'
#' @description Convert nutrients from g to umol (and vice versa)
#'
#' @param x Numeric value to convert
#' @param to Character specifying unit to convert to. Either "g" (gram) or "um" (micromole).
#'
#' @details
#' Converts the amount of nutrients from gram to micromole or the other way around.
#'
#' @return numeric
#'
#' @aliases int_convert_n
#' @rdname int_convert_n
#'
#' @keywords internal
#'
#' @export
int_convert_n <- function(x, to) {

  # MH: Where is the value 18.039 coming from?

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
