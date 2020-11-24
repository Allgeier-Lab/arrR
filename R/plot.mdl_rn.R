#' plot.mdl_rn
#'
#' @description Plotting method for mdl_rn object
#'
#' @param x mdl_rn object of simulation run.
#' @param what Character specifying what to plot.
#' @param summarize Character to specify which values of environmental data is used as fill.
#' @param timestep Integer to specify which timestep is plotted.
#' @param limits Named list with vectors with min and maximum value of values.
#' @param base_size Numeric to specify base font size.
#' @param ... Not used.
#'
#' @details
#' Plotting method for model run results simulated with \code{\link{run_simulation}}.
#'
#' @examples
#' # Add example code
#'
#' @aliases plot.mdl_rn
#' @rdname plot.mdl_rn
#'
#' @export
plot.mdl_rn <- function(x, what = "seafloor", summarize = FALSE, timestep = x$max_i, limits = NULL,
                        base_size = 10, ...) {

  i <- timestep

  # check if i can be divided by save_each without reminder
  if (i %% x$save_each != 0) {

    stop("'timestep' was not saved during model run.",
         call. = FALSE)
  }

  # plot summarized results
  if (summarize) {

    # summarize results
    summarised_result <- summarize_results(result = x)

    # plot seafloor
    if (what == "seafloor") {

      # get seafloor data
      seafloor <- subset(summarised_result$seafloor,
                         select = c("timestep", "summary",
                                    "ag_biomass", "bg_biomass",
                                    "nutrients_pool", "detritus_pool"))

      # create plot
      gg_top_left <- ggplot2::ggplot(data = seafloor) +
        ggplot2::geom_line(ggplot2::aes(x = timestep, y = ag_biomass,
                                        col = summary, linetype = summary)) +
        ggplot2::scale_y_continuous(limits = limits$ag_biomass) +
        ggplot2::scale_color_manual(values = c("grey", "black", "grey")) +
        ggplot2::scale_linetype_manual(values = c(2, 1, 2)) +
        ggplot2::guides(col = FALSE, linetype = FALSE) +
        ggplot2::labs(x = "Timestep", y = "Dry weight ag biomass [g/cell]") +
        ggplot2::theme_classic(base_size = base_size) +
        ggplot2::theme(plot.title = ggplot2::element_text(size = base_size))

      # create plot
      gg_top_right <- ggplot2::ggplot(data = seafloor) +
        ggplot2::geom_line(ggplot2::aes(x = timestep, y = bg_biomass,
                                        col = summary, linetype = summary)) +
        ggplot2::scale_y_continuous(limits = limits$bg_biomass) +
        ggplot2::scale_color_manual(values = c("grey", "black", "grey")) +
        ggplot2::scale_linetype_manual(values = c(2, 1, 2)) +
        ggplot2::guides(col = FALSE, linetype = FALSE) +
        ggplot2::labs(x = "Timestep", y = "Dry weight bg biomass [g/cell]") +
        ggplot2::theme_classic(base_size = base_size) +
        ggplot2::theme(plot.title = ggplot2::element_text(size = base_size))

      # create plot
      gg_bottom_left <- ggplot2::ggplot(data = seafloor) +
        ggplot2::geom_line(ggplot2::aes(x = timestep, y = nutrients_pool,
                                        col = summary, linetype = summary)) +
        ggplot2::scale_y_continuous(limits = limits$nutrients_pool) +
        ggplot2::scale_color_manual(values = c("grey", "black", "grey")) +
        ggplot2::scale_linetype_manual(values = c(2, 1, 2)) +
        ggplot2::guides(col = FALSE, linetype = FALSE) +
        ggplot2::labs(x = "Timestep", y = "Nutrients pool [g/cell]") +
        ggplot2::theme_classic(base_size = base_size) +
        ggplot2::theme(plot.title = ggplot2::element_text(size = base_size))

      # create plot
      gg_bottom_right <- ggplot2::ggplot(data = seafloor) +
        ggplot2::geom_line(ggplot2::aes(x = timestep, y = detritus_pool,
                                        col = summary, linetype = summary)) +
        ggplot2::scale_y_continuous(limits = limits$detritus_pool) +
        ggplot2::scale_color_manual(values = c("grey", "black", "grey")) +
        ggplot2::scale_linetype_manual(values = c(2, 1, 2)) +
        ggplot2::guides(col = FALSE, linetype = FALSE) +
        ggplot2::labs(x = "Timestep", y = "Detritus pool [g/cell]") +
        ggplot2::theme_classic(base_size = base_size) +
        ggplot2::theme(plot.title = ggplot2::element_text(size = base_size))

    # plot fish population
    } else if (what == "fish_population") {

      # get fish  data
      fish_population <- subset(summarised_result$fish_population,
                         select = c("timestep" ,"summary",
                                    "length", "weight",
                                    "died_consumption", "died_background"))

      # create plot
      gg_top_left <- ggplot2::ggplot(data = fish_population) +
        ggplot2::geom_line(ggplot2::aes(x = timestep, y = length,
                                        col = summary, linetype = summary)) +
        ggplot2::scale_color_manual(values = c("grey", "black", "grey")) +
        ggplot2::scale_linetype_manual(values = c(2, 1, 2)) +
        ggplot2::guides(col = FALSE, linetype = FALSE) +
        ggplot2::labs(x = "Timestep", y = "Body length [cm]") +
        ggplot2::theme_classic(base_size = base_size) +
        ggplot2::theme(plot.title = ggplot2::element_text(size = base_size))

      # create plot
      gg_top_right <- ggplot2::ggplot(data = fish_population) +
        ggplot2::geom_line(ggplot2::aes(x = timestep, y = weight,
                                        col = summary, linetype = summary)) +
        ggplot2::scale_color_manual(values = c("grey", "black", "grey")) +
        ggplot2::scale_linetype_manual(values = c(2, 1, 2)) +
        ggplot2::guides(col = FALSE, linetype = FALSE) +
        ggplot2::labs(x = "Timestep", y = "Body weigth [g]") +
        ggplot2::theme_classic(base_size = base_size) +
        ggplot2::theme(plot.title = ggplot2::element_text(size = base_size))

      # create plot
      gg_bottom_left <- ggplot2::ggplot(data = fish_population) +
        ggplot2::geom_line(ggplot2::aes(x = timestep, y = died_consumption,
                                        col = summary, linetype = summary)) +
        ggplot2::scale_color_manual(values = c("grey", "black", "grey")) +
        ggplot2::scale_linetype_manual(values = c(2, 1, 2)) +
        ggplot2::guides(col = FALSE, linetype = FALSE) +
        ggplot2::labs(x = "Timestep", y = "Count mortality consumption [#]") +
        ggplot2::theme_classic(base_size = base_size) +
        ggplot2::theme(plot.title = ggplot2::element_text(size = base_size))

      # create plot
      gg_bottom_right <- ggplot2::ggplot(data = fish_population) +
        ggplot2::geom_line(ggplot2::aes(x = timestep, y = died_background,
                                        col = summary, linetype = summary)) +
        ggplot2::scale_color_manual(values = c("grey", "black", "grey")) +
        ggplot2::scale_linetype_manual(values = c(2, 1, 2)) +
        ggplot2::guides(col = FALSE, linetype = FALSE) +
        ggplot2::labs(x = "Timestep", y = "Count mortality background [#]") +
        ggplot2::theme_classic(base_size = base_size) +
        ggplot2::theme(plot.title = ggplot2::element_text(size = base_size))

    # what doesn't make sense
    } else {

      stop("Please select 'seafloor' or 'fish_population' as 'what argument.",
           call. = FALSE)

    }

  # plot map
  } else {

    if (what == "seafloor") {

      # get seafloor data
      seafloor <- subset(x$seafloor, timestep == i,
                         select = c("x", "y",
                                    "ag_biomass", "bg_biomass",
                                    "nutrients_pool", "detritus_pool"))

      # create plot
      gg_top_left <- ggplot2::ggplot(data = seafloor) +
        ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = ag_biomass)) +
        ggplot2::scale_fill_gradientn(colours = c("#368AC0", "#F4B5BD", "#EC747F"),
                                      na.value = "#9B964A", limits = limits$ag_biomass,
                                      name = "Dry weight ag\nbiomass [g/cell]") +
        ggplot2::coord_equal() +
        ggplot2::labs(x = "", y = "") +
        ggplot2::theme_classic(base_size = base_size) +
        ggplot2::theme(plot.title = ggplot2::element_text(size = base_size))

      # create plot
      gg_top_right <- ggplot2::ggplot(data = seafloor) +
        ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = bg_biomass)) +
        ggplot2::scale_fill_gradientn(colours = c("#368AC0", "#F4B5BD", "#EC747F"),
                                      na.value = "#9B964A", limits = limits$bg_biomass,
                                      name = "Dry weight bg\nbiomass [g/cell]") +
        ggplot2::coord_equal() +
        ggplot2::labs(x = "", y = "") +
        ggplot2::theme_classic(base_size = base_size) +
        ggplot2::theme(plot.title = ggplot2::element_text(size = base_size))

      # create plot
      gg_bottom_left <- ggplot2::ggplot(data = seafloor) +
        ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = nutrients_pool)) +
        ggplot2::scale_fill_gradientn(colours = c("#368AC0", "#F4B5BD", "#EC747F"),
                                      na.value = "#9B964A", limits = limits$nutrients_pool,
                                      name = "Nutrients\npool [g/cell]") +
        ggplot2::coord_equal() +
        ggplot2::labs(x = "", y = "") +
        ggplot2::theme_classic(base_size = base_size) +
        ggplot2::theme(plot.title = ggplot2::element_text(size = base_size))

      # create plot
      gg_bottom_right <- ggplot2::ggplot(data = seafloor) +
        ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = detritus_pool)) +
        ggplot2::scale_fill_gradientn(colours = c("#368AC0", "#F4B5BD", "#EC747F"),
                                      na.value = "#9B964A", limits = limits$detritus_pool,
                                      name = "Detritus\nnutrients [g/cell]") +
        ggplot2::coord_equal() +
        ggplot2::labs(x = "", y = "") +
        ggplot2::theme_classic(base_size = base_size) +
        ggplot2::theme(plot.title = ggplot2::element_text(size = base_size))

    # plot fishpopulation
    } else if (what == "fish_population") {

      # get seafloor data
      fish_population <- get_density(x, timestep = i)

      # create plot
      gg_density <- ggplot2::ggplot(data = fish_population) +
        ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = density)) +
        ggplot2::scale_fill_gradientn(colours = c("#368AC0", "#F4B5BD", "#EC747F"),
                                      na.value = "#9B964A", name = "Density [#/cell]") +
        ggplot2::coord_equal() +
        ggplot2::theme_classic(base_size = base_size) +
        ggplot2::labs(title = paste0("Simulation time: ",
                                     round(i * x$min_per_i / 60 / 24, 1),
                                     " days\n(Timesteps: ", i, ")")) +
        ggplot2::theme(plot.title = ggplot2::element_text(size = base_size))

      return(gg_density)

    # what doesn't make sense
    } else {

      stop("Please select 'seafloor' or 'fish_population' as 'what argument.",
           call. = FALSE)

    }
  }

  # now add the title
  title <- cowplot::ggdraw() +
    cowplot::draw_label(label = paste0("Simulation time: ",
                                       round(i * x$min_per_i / 60 / 24, 1),
                                       " days\n(Timesteps: ", i, ")"),
                        x = 0, hjust = 0, size = base_size) +
    ggplot2::theme(plot.margin = ggplot2::margin(t = 0, r = 0,
                                                 b = 0, l = 1, "cm"))

  # combine to one grid
  gg_all <- cowplot::plot_grid(gg_top_left, gg_top_right,
                               gg_bottom_left, gg_bottom_right, nrow = 2, ncol = 2)

  # add title
  gg_all <- cowplot::plot_grid(title, gg_all, ncol = 1, rel_heights = c(0.1, 1))

  return(gg_all)

}
