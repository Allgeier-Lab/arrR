#' distribute_detritus
#'
#' @description Redistribute detritus pool
#'
#' @param seafloor_values Data.frame of seafloor values.
#' @param parameters List with all model parameters.
#'
#' @details
#' Function to redistribute dead detritus pool to overall detritus pool and decomposition.
#'
#' @references
#' DeAngelis, D.L., 1992. Dynamics of Nutrient Cycling and Food Webs. Springer
#' Netherlands, Dordrecht. https://doi.org/10.1007/978-94-011-2342-6
#'
#' @return RasterBrick
#'
#' @aliases distribute_dead_detritus
#' @rdname distribute_dead_detritus
#'
#' @export
distribute_detritus <- function(seafloor_values, parameters) {

  dead_decompostion <- seafloor_values$detritus_dead * parameters$detritus_dead_decomp

  # redistribute dead detritus to active detritus
  seafloor_values$detritus_pool <- seafloor_values$detritus_pool + dead_decompostion

  seafloor_values$detritus_dead <- seafloor_values$detritus_dead - dead_decompostion

  # get detritus amount that goes into nutrients pool
  decomposition <- seafloor_values$detritus_pool * parameters$detritus_decomposition

  # add to nutrients pool
  seafloor_values$nutrients_pool <- seafloor_values$nutrients_pool + decomposition

  # remove from detritus pool
  seafloor_values$detritus_pool <- seafloor_values$detritus_pool - decomposition

  return(seafloor_values)
}
