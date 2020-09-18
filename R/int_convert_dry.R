#' convert_dry
#'
#' @description Convert wet biomass to dry biomass
#'
#' @param x Numeric value to convert.
#' @param what Either 'above' or 'below'.
#'
#' @details
#' Converts above ground wet biomass to dry biomass.
#'
#' @return numeric
#'
#' @aliases int_int_convert_dry
#' @rdname int_int_convert_dry
#'
#' @keywords internal
#'
#' @export
int_convert_dry <- function(x, what) {

  # MH: Should these values be parameters?

  # convert above ground biomass
  if (what == "above") {

    result <- 0.3436 * x - 0.0213

  # convert below ground biomass
  } else if (what == "below") {

    result <- 0.0941 * x - 0.0396

  }

  # no valid option chosen
  else {

    stop("Please selecht either what = 'above' or what = 'below'",
         call. = FALSE)

  }

  result <- ifelse(result < 0, yes = 0, no = result)

  return(result)
}
