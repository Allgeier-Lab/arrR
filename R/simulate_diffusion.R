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

  seafloor_values <- rcpp_diffuse_values(seafloor_values = as.matrix(seafloor_values),
                                         cell_adj = cell_adj,
                                         nutrients_diffusion = parameters$nutrients_diffusion,
                                         detritus_diffusion = parameters$detritus_diffusion,
                                         detritus_dead_diffusion = parameters$detritus_dead_diffusion)

  return(as.data.frame(seafloor_values))

}
