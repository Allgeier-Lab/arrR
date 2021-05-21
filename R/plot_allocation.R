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

  # new "minimum" of sigmoid function
  bg_biomass_min_temp <- parameters$bg_biomass_min +
    (parameters$bg_biomass_max - parameters$bg_biomass_min) * parameters$seagrass_thres

  # create vector with bg values
  x <- seq(from = bg_biomass_min_temp, to = parameters$bg_biomass_max, by = 0.1)

  # scale to 0 - 1
  x_scl <- (x - bg_biomass_min_temp) / (parameters$bg_biomass_max - bg_biomass_min_temp)

  # calculate midpoint
  midpoint <- -log(2) / log(parameters$seagrass_thres)

  # calculate allocation ratio
  y <- 1 / (1 + (x_scl ^ midpoint / (1 - x_scl ^ midpoint)) ^ -parameters$seagrass_slope)

  # adding original minimum to x
  x <- c(parameters$bg_biomass_min, x)

  # adding ratio below threshold
  y <- c(0, y)

  # set x axis breaks
  breaks <- seq(from = parameters$bg_biomass_min, to = parameters$bg_biomass_max,
                length.out = 5)

  # create plot
  gg <- ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x = x, y = y,  col = "Aboveground")) +
    ggplot2::geom_line(ggplot2::aes(x = x, y = 1 - y, col = "Belowground")) +
    ggplot2::geom_vline(xintercept = bg_biomass_min_temp, linetype = 2, col = "grey85") +
    ggplot2::labs(x = "Belowground biomass (min to max)", y = "Ratio of nutrient uptake allocation",
                  subtitle = paste0("Threshold: ", round(parameters$seagrass_thres, 2),
                                    "; Slope: ", round(parameters$seagrass_slope, 2))) +
    ggplot2::scale_color_viridis_d(name = "Biomass") +
    ggplot2::scale_x_continuous(limits = c(parameters$bg_biomass_min, parameters$bg_biomass_max),
                                breaks = breaks) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "bottom")

  return(gg)
}
