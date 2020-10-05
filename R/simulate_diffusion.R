#' simulate_diffusion
#'
#' @description Simulate diffusion.
#'
#' @param seafloor_values Data.frame of seafloor values.
#' @param cell_adj 2 column matrix with cell adjacencies.
#' @param parameters List with all model parameters.
#'
#' @details
#' A certain share of each cell value is diffused to its 8 neighboring cells.
#'
#' @return RasterBrick
#'
#' @aliases simulate_diffusion
#' @rdname simulate_diffusion
#'
#' @export
simulate_diffusion <- function(seafloor_values, cell_adj, parameters) {

  # get id of focal cell and neighboring cell
  id_from <- cell_adj[, 1]

  id_to <- cell_adj[, 2]

  # randomize row id to avoid strange patterns
  random_id <- sample(x = 1:length(id_from), size = length(id_from))

  # reorder values
  id_from[random_id] <- id_from

  id_to[random_id] <- id_to

  # get amount of nutrients that are diffused
  seafloor_diff <- as.matrix(seafloor_values[, c("wc_nutrients", "detritus_pool",
                                                 "detritus_dead")]) %*%
    diag(c(parameters$wc_diffusion,
           parameters$detritus_diffusion,
           parameters$detritus_death_diffusion))

  # add diffusion values to neighboring cells
  seafloor_values[id_to, ] <- seafloor_values[id_to, ] + (seafloor_diff[id_from, ] / 8)

  # remove diffusion values from focal cell
  seafloor_values <- seafloor_values - (seafloor_diff / 8)

  return(seafloor_values)
}
