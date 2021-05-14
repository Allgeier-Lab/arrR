#' simulate_mineralization
#'
#' @description Redistribute detritus pools
#'
#' @param seafloor_values Matrix of seafloor values.
#' @param parameters List with all model parameters.
#'
#' @details
#' Function to redistribute fish detritus pool to overall detritus pool and decomposition.
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

  rcpp_mineralization(seafloor = seafloor_values,
                      detritus_fish_ratio = parameters$detritus_fish_ratio,
                      detritus_mineralization = parameters$detritus_mineralization)

}
