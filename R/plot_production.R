#' plot_production
#'
#' @description
#' Plotting method.
#'
#' @param result mdl_rn object.
#' @param lag Logical if TRUE, cumulative or difference to previous time step should be returned.
#'
#' @details
#' Plotting method for \code{mdl_rn} object simulated with \code{\link{run_simulation}}
#' for bg and ag biomass production.
#'
#' @examples
#' \dontrun{
#' plot_production(result = result_rand)
#' }
#'
#' @aliases plot_production
#' @rdname plot_production
#'
#' @export
plot_production <- function(result, lag = TRUE) {

  # get production values
  production_temp <- get_production(result = result, lag = lag)

  # remove NA row
  if (lag == TRUE) {

    production_temp <- production_temp[-1, ]

  }

  # convert them to long format
  production_temp <- stats::reshape(data = production_temp, direction = "long",
                                    v.names = "value", varying = list(names(production_temp[, -1])),
                                    idvar = "timestep", ids = production_temp[, 1],
                                    timevar = "part", times = names(production_temp[, -1]),
                                    new.row.names = seq(from = 1, to = nrow(production_temp) * 2))

  # create ggplot
  gg_prod <- ggplot2::ggplot(data = production_temp) +
    ggplot2::geom_line(ggplot2::aes(x = .data$timestep, y = .data$value)) +
    ggplot2::facet_wrap(. ~ part, scales = "free_y", nrow = 2, ncol = 1) +
    ggplot2::labs(x = "Time step", y = "Production") +
    ggplot2::scale_color_viridis_d(name = "") +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "bottom")

  return(gg_prod)
}
