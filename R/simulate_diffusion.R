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

  # # randomize row id to avoid strange patterns
  random_id <- sample(x = 1:nrow(cell_adj), size = nrow(cell_adj))

  # reorder values
  id_from <- cell_adj[random_id, 1]

  id_to <- cell_adj[random_id, 2]

  # get amount of values that are diffused
  diff_nutrients <- seafloor_values$wc_nutrients * parameters$wc_diffusion

  diff_detritus <- seafloor_values$detritus_pool * parameters$detritus_diffusion

  diff_detritus_dead <- seafloor_values$detritus_dead * parameters$detritus_dead_diffusion

  # add diffusion values to neighboring cells
  seafloor_values$wc_nutrients[id_to] <- seafloor_values$wc_nutrients[id_to] +
    (diff_nutrients[id_from] / 8)

  seafloor_values$detritus_pool[id_to] <- seafloor_values$detritus_pool[id_to] +
    (diff_detritus[id_from] / 8)

  seafloor_values$detritus_dead[id_to] <- seafloor_values$detritus_dead[id_to] +
    (diff_detritus_dead[id_from] / 8)

  # remove diffusion values from focal cell
  seafloor_values$wc_nutrients[id_from] <- seafloor_values$wc_nutrients[id_from] -
    (diff_nutrients[id_from] / 8)

  seafloor_values$detritus_pool[id_from] <- seafloor_values$detritus_pool[id_from] -
    (diff_detritus[id_from] / 8)

  seafloor_values$detritus_dead[id_from] <- seafloor_values$detritus_dead[id_from] -
    (diff_detritus_dead[id_from] / 8)

  return(seafloor_values)
}
