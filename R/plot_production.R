#' plot_production
#'
#' @description
#' Plotting method.
#'
#' @param result mdl_rn object.
#' @param summarize Logical if values over time should be plotted.
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
plot_production <- function(result, summarize = FALSE, lag = TRUE) {

  if (summarize) {

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

    plot_title <- paste0("Total time : ", result$max_i, " iterations (",
                         round(result$max_i * result$min_per_i / 60 / 24, 1), " days)",
                         "\nFishpop    : ", result$starting_values$pop_n," indiv (movement: '", result$movement, "')")


    # create ggplot
    gg_prod <- ggplot2::ggplot(data = production_temp) +
      ggplot2::geom_line(ggplot2::aes(x = .data$timestep, y = .data$value)) +
      ggplot2::facet_wrap(. ~ part, scales = "free_y", nrow = 2, ncol = 1) +
      ggplot2::labs(x = "Time step", y = "Production", title = plot_title) +
      ggplot2::scale_color_viridis_d(name = "") +
      ggplot2::theme_classic() +
      ggplot2::theme(legend.position = "bottom")

  } else {

    # get seafloor data
    seafloor <- result$seafloor[result$seafloor$timestep == max(result$seafloor$timestep),
                           c("x", "y", "ag_production", "bg_production",
                             "ag_slough", "bg_slough")]

    # get name of columns
    col_names <- names(seafloor)

    # create plot
    gg_top_left <- ggplot2::ggplot(data = seafloor) +
      ggplot2::geom_raster(ggplot2::aes(x = .data$x, y = .data$y, fill = !!ggplot2::sym(col_names[3]))) +
      ggplot2::scale_fill_gradientn(colours = c("#368AC0", "#F4B5BD", "#EC747F"),
                                    na.value = "#9B964A", name = col_names[3]) +
      ggplot2::coord_equal() +
      ggplot2::labs(x = "", y = "") +
      ggplot2::theme_classic()

    # create plot
    gg_top_right <- ggplot2::ggplot(data = seafloor) +
      ggplot2::geom_raster(ggplot2::aes(x = .data$x, y = .data$y, fill = !!ggplot2::sym(col_names[4]))) +
      ggplot2::scale_fill_gradientn(colours = c("#368AC0", "#F4B5BD", "#EC747F"),
                                    na.value = "#9B964A", name = col_names[4]) +
      ggplot2::coord_equal() +
      ggplot2::labs(x = "", y = "") +
      ggplot2::theme_classic()

    # create plot
    gg_bottom_left <- ggplot2::ggplot(data = seafloor) +
      ggplot2::geom_raster(ggplot2::aes(x = .data$x, y = .data$y, fill = !!ggplot2::sym(col_names[5]))) +
      ggplot2::scale_fill_gradientn(colours = c("#368AC0", "#F4B5BD", "#EC747F"),
                                    na.value = "#9B964A", name = col_names[5]) +
      ggplot2::coord_equal() +
      ggplot2::labs(x = "", y = "") +
      ggplot2::theme_classic()

    # create plot
    gg_bottom_right <- ggplot2::ggplot(data = seafloor) +
      ggplot2::geom_raster(ggplot2::aes(x = .data$x, y = .data$y, fill = !!ggplot2::sym(col_names[6]))) +
      ggplot2::scale_fill_gradientn(colours = c("#368AC0", "#F4B5BD", "#EC747F"),
                                    na.value = "#9B964A", name = col_names[6]) +
      ggplot2::coord_equal() +
      ggplot2::labs(x = "", y = "") +
      ggplot2::theme_classic()

    # create title
    plot_title <- paste0("Total time : ", result$max_i, " iterations (",
                         round(result$max_i * result$min_per_i / 60 / 24, 1), " days)",
                         "\nFishpop    : ", result$starting_values$pop_n," indiv (movement: '", result$movement, "')")

    # now add the title
    title <- cowplot::ggdraw() +
      cowplot::draw_label(label = plot_title, x = 0, hjust = 0) +
      ggplot2::theme(plot.margin = ggplot2::margin(t = 0, r = 0,
                                                   b = 0, l = 1, "cm"))

    # combine to one grid
    gg_all <- cowplot::plot_grid(gg_top_left, gg_top_right,
                                 gg_bottom_left, gg_bottom_right, nrow = 2, ncol = 2)

    # add title
    gg_prod <- cowplot::plot_grid(title, gg_all, ncol = 1, rel_heights = c(0.1, 1))

  }

  return(gg_prod)
}
