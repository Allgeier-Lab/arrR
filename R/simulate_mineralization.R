#' simulate_mineralization
#'
#' @description Redistribute detritus pools
#'
#' @param seafloor_values Matrix of seafloor values.
#' @param parameters List with all model parameters.
#'
#' @details
#' Function to redistribute dead detritus pool to overall detritus pool and decomposition.
#'
#' @references
#' DeAngelis, D.L., 1992. Dynamics of Nutrient Cycling and Food Webs. Springer
#' Netherlands, Dordrecht. https://doi.org/10.1007/978-94-011-2342-6
#'
#' @return Matrix
#'
#' @aliases simulate_mineralization
#' @rdname simulate_mineralization
#'
#' @export
simulate_mineralization <- function(seafloor_values, parameters) {

  # calculate decomposition amount
  dead_decompostion <- seafloor_values[, "detritus_dead"] * parameters$detritus_dead_decomp

  # redistribute dead detritus to active detritus
  seafloor_values[, "detritus_pool"] <- seafloor_values[, "detritus_pool"] + dead_decompostion

  seafloor_values[, "detritus_dead"] <- seafloor_values[, "detritus_dead"] - dead_decompostion

  # get detritus amount that goes into nutrients pool
  mineralization <- seafloor_values[, "detritus_pool"] * parameters$detritus_mineralization

  # add to nutrients pool
  seafloor_values[, "nutrients_pool"] <- seafloor_values[, "nutrients_pool"] + mineralization

  # remove from detritus pool
  seafloor_values[, "detritus_pool"]  <- seafloor_values[, "detritus_pool"]  - mineralization

  return(seafloor_values)
}
