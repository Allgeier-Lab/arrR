#' get_seafloor_dim
#'
#' @description
#' Setup seafloor for model run.
#'
#' @param seafloor Data.frame with seafloor values
#'
#' @details
#' Returns dimensions, extent, and grain of seafloor data.frame.
#'
#' @return list
#'
#' @examples
#' \dontrun{
#' reef <- matrix(data = c(-1, 0, 0, 1, 1, 0, 0, -1, 0, 0),
#' ncol = 2, byrow = TRUE)
#'
#' seafloor <- setup_seafloor(dimensions = c(50, 50), grain = 1,
#' reef = reef, starting_values = default_starting)
#'
#' get_seafloor_dim(seafloor = seafloor)
#' }
#'
#' @aliases get_seafloor_dim
#' @rdname get_seafloor_dim
#'
#' @keywords internal
get_seafloor_dim <- function(seafloor) {

  # get dimensions of environment (nrow, ncol)
  dimensions <- c(nrow = length(unique(seafloor$y)), ncol = length(unique(seafloor$x)))

  # get grain by unique difference between coords without "jump" to new col/row
  grain <- unique(abs(diff(seafloor$x)[diff(seafloor$x) > 0]))

  # get extent of seafloor
  extent <- c(xmin = min(seafloor$x) - grain / 2, xmax = max(seafloor$x) + grain / 2,
              ymin = min(seafloor$y) - grain / 2, ymax = max(seafloor$y) + grain / 2)

  return(list(dimensions = dimensions, extent = extent, grain = grain))
}
