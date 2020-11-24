#' simulate_diffusion
#'
#' @description Simulate diffusion.
#'
#' @param seafloor_values Data.frame of seafloor values.
#' @param cell_adj 2-column matrix with cell adjacencies.
#' @param parameters List with all model parameters.
#'
#' @details
#' A certain share of each cell value, specified by the diffusion parameters, is
#' diffused to its 8 neighboring cells.
#'
#' @return data.frame
#'
#' @aliases simulate_diffusion
#' @rdname simulate_diffusion
#'
#' @export
simulate_diffusion <- function(seafloor_values, cell_adj, parameters) {

  # convert to matrix for rcpp
  seafloor_values_mat <- cbind(seafloor_values$nutrients_pool,
                               seafloor_values$detritus_pool,
                               seafloor_values$detritus_dead)

  # diffuse values and save result
  values_diffused <- rcpp_diffuse_values(seafloor_values = seafloor_values_mat,
                                         cell_adj = cell_adj,
                                         nutrients_diffusion = parameters$nutrients_diffusion,
                                         detritus_diffusion = parameters$detritus_diffusion,
                                         detritus_dead_diffusion = parameters$detritus_dead_diffusion)

  # update values
  seafloor_values$nutrients_pool <- values_diffused[, 1]
  seafloor_values$detritus_pool <- values_diffused[, 2]
  seafloor_values$detritus_dead <- values_diffused[, 3]

  return(seafloor_values)

}
