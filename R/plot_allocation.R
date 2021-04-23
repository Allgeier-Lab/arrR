#' plot_allocation
#'
#' @description Plotting method allocation
#'
#' @param parameters List with model parameters.
#'
#' @details
#' Plots the allocation ratio for given parameters
#'
#' @examples
#' # Add example code
#'
#' @aliases plot_allocation
#' @rdname plot_allocation
#'
#' @export
plot_allocation <- function(parameters) {

  # create vector with bg valuues
  x <- seq(from = parameters$bg_biomass_min, to = parameters$bg_biomass_max, by = 0.1)

  # scale to 0-1
  x_scl <- (x - parameters$bg_biomass_min) /
    (parameters$bg_biomass_max - parameters$bg_biomass_min);

  # calculate midpoint
  midpoint <- -log(2) / log(parameters$seagrass_thres)

  # calculate allocation ratio
  y <- 1 / (1 + (x_scl ^ midpoint / (1 - x_scl ^ midpoint)) ^ -parameters$seagrass_slope)

  # create plot
  gg <- ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x = x, y = y,  col = "Aboveground")) +
    ggplot2::geom_line(ggplot2::aes(x = x, y = 1 - y, col = "Belowground")) +
    ggplot2::labs(x = "Belowground biomass (min to max)", y = "Ratio of nutrient uptake allocation") +
    ggplot2::scale_color_viridis_d(name = "Biomass") +
    ggplot2::theme_bw()

    return(gg)
}
