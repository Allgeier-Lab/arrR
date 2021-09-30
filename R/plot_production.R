#' plot_production
#'
#' @description
#' Plotting method for production.
#'
#' @param result mdl_rn object of simulation run.
#' @param lag Logical if cumulative or difference to previous timestep should be returned.
#' @param base_size Numeric to specify base font size.
#'
#' @details
#' Plotting method for ag and bg production of the seagrass growth.
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
plot_production <- function(result, lag = TRUE, base_size = 10) {

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
    ggplot2::geom_line(ggplot2::aes(x = timestep, y = value)) +
    ggplot2::facet_wrap(. ~ part, scales = "free_y", nrow = 2, ncol = 1) +
    ggplot2::labs(x = "Timestep", y = "Production") +
    ggplot2::scale_color_viridis_d(name = "") +
    ggplot2::theme_classic(base_size = base_size) +
    ggplot2::theme(legend.position = "bottom")

  return(gg_prod)
}
