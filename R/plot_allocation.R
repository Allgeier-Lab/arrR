#' plot_allocation
#'
#' @description
#' Plotting method.
#'
#' @param parameters List with model parameters.
#'
#' @details
#' Plotting method for the allocation ratio rule used during the nutrient uptake
#' allocation within the seagrass dynamics. For more information see \code{rcpp_seagrass_growth}
#' and \code{rcpp_allocation_ratio} (internal functions).
#'
#' @examples
#' plot_allocation(parameters = arrR_parameters)
#'
#' @aliases plot_allocation
#' @rdname plot_allocation
#'
#' @export
plot_allocation <- function(parameters) {

  # create vector with bg values
  bg_biomass <- seq(from = parameters$bg_biomass_min,
                    to = parameters$bg_biomass_max, by = 0.1)

  # calc ratios
  ratio <- vapply(bg_biomass, function(x)
    rcpp_allocation_ratio(x, biomass_min = parameters$bg_biomass_min, biomass_max = parameters$bg_biomass_max,
                          threshold = parameters$seagrass_thres, slope = parameters$seagrass_slope),
    FUN.VALUE = numeric(1))

  # calc threshold
  threshold_temp <- parameters$bg_biomass_min +
    (parameters$bg_biomass_max - parameters$bg_biomass_min) * abs(parameters$seagrass_thres)

  # set x axis breaks
  breaks <- seq(from = parameters$bg_biomass_min, to = parameters$bg_biomass_max,
                length.out = 5)

  # create plot
  gg <- ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x = bg_biomass, y = 1 - ratio,  col = "Aboveground")) +
    ggplot2::geom_line(ggplot2::aes(x = bg_biomass, y = ratio, col = "Belowground")) +
    ggplot2::geom_vline(xintercept = threshold_temp, linetype = 2, col = "grey85") +
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
