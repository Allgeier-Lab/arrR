#' simulate_diffusion
#'
#' @description Simulate diffusion.
#'
#' @param seafloor_values Matrix with seafloor values.
#' @param cell_adj 2-column matrix with cell adjacencies.
#' @param parameters List with all model parameters.
#'
#' @details
#' A certain share of each cell value, specified by the diffusion parameters, is
#' diffused to its 8 neighboring cells.
#'
#' @return matrix
#'
#' @aliases simulate_diffusion
#' @rdname simulate_diffusion
#'
#' @export
simulate_diffusion <- function(seafloor_values, cell_adj, parameters) {

  # diffuse values and save result
  rcpp_diffuse_values(seafloor = seafloor_values,
                      cell_adj = cell_adj,
                      nutrients_diffusion = parameters$nutrients_diffusion,
                      detritus_diffusion = parameters$detritus_diffusion,
                      detritus_fish_diffusion = parameters$detritus_fish_diffusion)

}
