#' distribute_detritus
#'
#' @description Redistribute dead detritus pool
#'
#' @param seafloor_values Data.frame of seafloor values.
#' @param parameters List with all model parameters.
#'
#' @details
#' Function to redistribute dead detritus pool to overall detritus pool
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

  # update values
  seafloor_values$detritus_pool <- seafloor_values$detritus_pool +
    (seafloor_values$detritus_dead * parameters$detritus_dead_decomp)

  seafloor_values$detritus_dead <- seafloor_values$detritus_dead -
    (seafloor_values$detritus_dead * parameters$detritus_dead_decomp)

  return(seafloor_values)
}
