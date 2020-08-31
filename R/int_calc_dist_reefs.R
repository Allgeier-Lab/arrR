#' calc_dist_reefs
#'
#' @description Internal function
#'
#' @param fish_population 2-column matrix with coordinates of individual fish.
#' @param coords_reef 2-column matrix with coordinates of AR.
#'
#' @details
#' Internal function calculate distance to reef cells.
#'
#' @return vector
#'
#' @aliases calc_dist_reefs
#' @rdname calc_dist_reefs
#'
#' @keywords internal
#'
#' @export
int_calc_dist_reefs <- function(fish_population, coords_reef) {

  min_dist <- rep(Inf, times = nrow(fish_population))

  counter <- vector(mode = "numeric", length = nrow(fish_population))

  for (i in 1:nrow(fish_population)) {

    for (j in 1:nrow(coords_reef)) {

      dist_x <- fish_population[i, 1] - coords_reef[j, 1]

      dist_y <- fish_population[i, 2] - coords_reef[j, 2]

      dist_xy <- sqrt(dist_x * dist_x + dist_y * dist_y)

      if (dist_xy < min_dist[i]) {

        min_dist[i] <- dist_xy

        counter[i] <- j

      }
    }
  }

  return(list(dist = min_dist, counter = counter))
}

